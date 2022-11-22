use crate::backend::binaryen;
use crate::entity::EntityRef;
use crate::ir::*;
use crate::Operator;
use fxhash::FxHashMap;
use std::collections::BTreeMap;

/// Creates a body expression for a function. Returns that expression,
/// and new locals (as their types) that were created as temporaries
/// and need to be appended to `body.locals`.
pub(crate) fn generate_body(
    module: &Module,
    body: &FunctionBody,
    into_mod: &mut binaryen::Module,
) -> (Vec<Type>, binaryen::Expression) {
    let mut ctx = ElabCtx::new(module, body);

    // For each block, generate an expr.
    let mut block_exprs: BTreeMap<Block, binaryen::Expression> = BTreeMap::default();
    for (block_id, block) in body.blocks.entries() {
        let mut exprs = vec![];
        for (i, (ty, _param)) in block.params.iter().enumerate() {
            let val = binaryen::Expression::local_get(
                into_mod,
                *ctx.block_param_next_locals.get(&(block_id, i)).unwrap(),
                *ty,
            );
            let set = binaryen::Expression::local_set(
                into_mod,
                *ctx.block_param_locals.get(&(block_id, i)).unwrap(),
                val,
            );
            exprs.push(set);
        }
        for &inst in &block.insts {
            let inst = body.resolve_alias(inst);
            if let Some(expr) = ctx.elaborate_value(into_mod, inst) {
                exprs.push(expr);
            }
        }
        block_exprs.insert(block_id, binaryen::Expression::block(into_mod, &exprs[..]));
    }

    // Combine blocks into a single body expression, using the
    // relooper/stackifier support built into Binaryen.
    let mut relooper = binaryen::Relooper::new(into_mod);

    // Create the blocks.
    let mut relooper_blocks: FxHashMap<Block, (binaryen::Expression, binaryen::RelooperBlock)> =
        FxHashMap::default();
    for (block_id, block_expr) in &mut block_exprs {
        let block = match &body.blocks[*block_id].terminator {
            &Terminator::Select { value, .. } => {
                let sel = ctx.get_val(value, into_mod);
                relooper.add_block_with_switch(block_expr.clone(), sel)
            }
            _ => relooper.add_block(block_expr.clone()),
        };
        relooper_blocks.insert(*block_id, (block_expr.clone(), block));
    }

    // Add edges.
    for &block_id in block_exprs.keys() {
        let (mut block_expr, block) = relooper_blocks.get(&block_id).unwrap().clone();
        match &body.blocks[block_id].terminator {
            &Terminator::Br { ref target } => {
                let (target_block, edge) = build_ssa_edge(&ctx, target, &relooper_blocks, into_mod);
                block.branch(target_block, edge);
            }
            &Terminator::CondBr {
                cond,
                ref if_true,
                ref if_false,
            } => {
                let (true_block, true_edge) =
                    build_ssa_edge(&ctx, if_true, &relooper_blocks, into_mod);
                let (false_block, false_edge) =
                    build_ssa_edge(&ctx, if_false, &relooper_blocks, into_mod);
                let cond = ctx.get_val(cond, into_mod);
                block.cond_branch(true_block, cond, true_edge);
                block.branch(false_block, false_edge);
            }
            &Terminator::Select {
                value: _,
                ref targets,
                ref default,
            } => {
                for (i, target) in targets.iter().enumerate() {
                    let (target_block, edge) =
                        build_ssa_edge(&ctx, target, &relooper_blocks, into_mod);
                    block.switch(target_block, edge, &[i as u32]);
                }
                let (target_block, edge) =
                    build_ssa_edge(&ctx, default, &relooper_blocks, into_mod);
                block.switch(target_block, edge, &[]);
            }
            &Terminator::Return { ref values } => {
                let values = values
                    .iter()
                    .map(|value| ctx.get_val(*value, into_mod))
                    .collect::<Vec<_>>();
                block_expr.block_append_child(binaryen::Expression::ret(into_mod, &values[..]));
            }
            &Terminator::Unreachable | &Terminator::None => {
                block_expr.block_append_child(binaryen::Expression::unreachable(into_mod));
            }
        }
    }

    let index_var = ctx.new_local(Type::I32);
    let entry = relooper_blocks.get(&ctx.body.entry).unwrap().1.clone();
    let expr = relooper.construct(entry, index_var.index());
    (ctx.new_locals, expr)
}

fn build_ssa_edge(
    ctx: &ElabCtx<'_>,
    target: &BlockTarget,
    blocks: &FxHashMap<Block, (binaryen::Expression, binaryen::RelooperBlock)>,
    into_mod: &mut binaryen::Module,
) -> (binaryen::RelooperBlock, binaryen::Expression) {
    // Copy all block args to the "next" locals. Build an edge block
    // with these get-set pairs.
    let mut sets = vec![];
    for (i, arg) in target.args.iter().enumerate() {
        let value = ctx.get_val(*arg, into_mod);
        let set = binaryen::Expression::local_set(
            into_mod,
            *ctx.block_param_next_locals.get(&(target.block, i)).unwrap(),
            value,
        );
        sets.push(set);
    }

    let edge_block = binaryen::Expression::block(into_mod, &sets[..]);
    let block = blocks.get(&target.block).unwrap().1.clone();
    (block, edge_block)
}

#[derive(Clone, Debug)]
struct ElabCtx<'a> {
    module: &'a Module<'a>,
    body: &'a FunctionBody,
    op_result_locals: FxHashMap<(Value, usize), Local>,
    block_param_locals: FxHashMap<(Block, usize), Local>,
    block_param_next_locals: FxHashMap<(Block, usize), Local>,
    new_locals: Vec<Type>,
}

impl<'a> ElabCtx<'a> {
    fn new(module: &'a Module<'a>, body: &'a FunctionBody) -> ElabCtx<'a> {
        let mut this = ElabCtx {
            module,
            body,
            op_result_locals: FxHashMap::default(),
            block_param_locals: FxHashMap::default(),
            block_param_next_locals: FxHashMap::default(),
            new_locals: vec![],
        };

        // Create operator result locals.
        for (value, def) in body.values.entries() {
            for (i, &ty) in def.tys().iter().enumerate() {
                let new_local = this.new_local(ty);
                this.op_result_locals.insert((value, i), new_local);
            }
        }

        // Create blockparam cur-value and next-value locals.
        for (block, def) in body.blocks.entries() {
            for (param, &(ty, _)) in def.params.iter().enumerate() {
                let cur_value = this.new_local(ty);
                let next_value = this.new_local(ty);
                this.block_param_locals.insert((block, param), cur_value);
                this.block_param_next_locals
                    .insert((block, param), next_value);
            }
        }

        this
    }

    fn elaborate_value(
        &mut self,
        into_mod: &binaryen::Module,
        value: Value,
    ) -> Option<binaryen::Expression> {
        let value = self.body.resolve_alias(value);

        match &self.body.values[value] {
            &ValueDef::Operator(op, ref args, ref tys) => {
                // Get expressions for each arg.
                let args = args.iter().map(|&arg| self.get_val_local(arg));
                // Create `get_local` expressions for each arg.
                let binaryen_args = args
                    .map(|arg_local| {
                        binaryen::Expression::local_get(
                            into_mod,
                            arg_local,
                            self.local_ty(arg_local),
                        )
                    })
                    .collect::<Vec<_>>();
                // Create operator.
                let expr = self.create_binaryen_op(op, binaryen_args, tys, into_mod);

                // Set local(s) as appropriate.
                if tys.len() == 0 {
                    // Nothing. Create a `drop` expr that wraps the actual operator.
                    Some(binaryen::Expression::expr_drop(into_mod, expr))
                } else if tys.len() == 1 {
                    // Set value directly.
                    let local = self.get_val_local(value);
                    Some(binaryen::Expression::local_set(into_mod, local, expr))
                } else {
                    todo!("support multivalue")
                }
            }
            _ => None,
        }
    }

    fn get_val_local(&self, value: Value) -> Local {
        match &self.body.values[value] {
            &ValueDef::BlockParam(block, idx, _) => {
                self.block_param_locals.get(&(block, idx)).copied().unwrap()
            }
            &ValueDef::Operator(..) => self.op_result_locals.get(&(value, 0)).copied().unwrap(),
            &ValueDef::PickOutput(value, idx, _) => {
                self.op_result_locals.get(&(value, idx)).copied().unwrap()
            }
            &ValueDef::Alias(val) => self.get_val_local(val),
            &ValueDef::Placeholder(_) | &ValueDef::None => unreachable!(),
        }
    }

    fn get_val(&self, value: Value, into_mod: &mut binaryen::Module) -> binaryen::Expression {
        let local = self.get_val_local(value);
        binaryen::Expression::local_get(into_mod, local, self.body.values[value].ty().unwrap())
    }

    fn new_local(&mut self, ty: Type) -> Local {
        let index = Local::new(self.body.locals.len() + self.new_locals.len());
        self.new_locals.push(ty);
        index
    }

    fn create_binaryen_op(
        &mut self,
        op: Operator,
        args: Vec<binaryen::Expression>,
        tys: &[Type],
        into_mod: &binaryen::Module,
    ) -> binaryen::Expression {
        match op {
            Unreachable => binaryen::Expression::unreachable(into_mod),
            Nop => binaryen::Expression::nop(into_mod),

            Operator::Call { function_index } => {
                binaryen::Expression::call(into_mod, function_index, &args[..], tys)
            }
            Operator::CallIndirect {
                sig_index,
                table_index,
            } => binaryen::Expression::call_indirect(
                into_mod,
                table_index,
                self.module.signature(sig_index),
                args[0],
                &args[1..],
            ),
            Operator::Return => binaryen::Expression::ret(into_mod, &args[..]),
            Operator::LocalSet { local_index } => {
                binaryen::Expression::local_set(into_mod, local_index, args[0])
            }
            Operator::LocalTee { local_index } => {
                binaryen::Expression::local_tee(into_mod, local_index, args[0], tys[0])
            }
            Operator::LocalGet { local_index } => {
                binaryen::Expression::local_get(into_mod, local_index, tys[0])
            }
            Operator::Select | Operator::TypedSelect { .. } => {
                binaryen::Expression::select(into_mod, args[0], args[1], args[2], tys[0])
            }
            Operator::GlobalGet { global_index } => {
                binaryen::Expression::global_get(into_mod, global_index, tys[0])
            }
            Operator::GlobalSet { global_index } => {
                binaryen::Expression::global_set(into_mod, global_index, args[0])
            }

            Operator::I32Load { memory } => binaryen::Expression::load(
                into_mod,
                4,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I64Load { memory } => binaryen::Expression::load(
                into_mod,
                8,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::F32Load { memory } => binaryen::Expression::load(
                into_mod,
                4,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::F64Load { memory } => binaryen::Expression::load(
                into_mod,
                8,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I32Load8S { memory } => binaryen::Expression::load(
                into_mod,
                1,
                true,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I32Load8U { memory } => binaryen::Expression::load(
                into_mod,
                1,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I32Load16S { memory } => binaryen::Expression::load(
                into_mod,
                2,
                true,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I32Load16U { memory } => binaryen::Expression::load(
                into_mod,
                2,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I64Load8S { memory } => binaryen::Expression::load(
                into_mod,
                1,
                true,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I64Load8U { memory } => binaryen::Expression::load(
                into_mod,
                1,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I64Load16S { memory } => binaryen::Expression::load(
                into_mod,
                2,
                true,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I64Load16U { memory } => binaryen::Expression::load(
                into_mod,
                2,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I64Load32S { memory } => binaryen::Expression::load(
                into_mod,
                4,
                true,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),
            Operator::I64Load32U { memory } => binaryen::Expression::load(
                into_mod,
                4,
                false,
                memory.offset,
                memory.align,
                tys[0],
                args[0],
                memory.memory,
            ),

            Operator::I32Store { memory } => binaryen::Expression::store(
                into_mod,
                4,
                memory.offset,
                memory.align,
                Type::I32,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::I64Store { memory } => binaryen::Expression::store(
                into_mod,
                8,
                memory.offset,
                memory.align,
                Type::I64,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::F32Store { memory } => binaryen::Expression::store(
                into_mod,
                4,
                memory.offset,
                memory.align,
                Type::F32,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::F64Store { memory } => binaryen::Expression::store(
                into_mod,
                8,
                memory.offset,
                memory.align,
                Type::F64,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::I32Store8 { memory } => binaryen::Expression::store(
                into_mod,
                1,
                memory.offset,
                memory.align,
                Type::I32,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::I32Store16 { memory } => binaryen::Expression::store(
                into_mod,
                2,
                memory.offset,
                memory.align,
                Type::I32,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::I64Store8 { memory } => binaryen::Expression::store(
                into_mod,
                1,
                memory.offset,
                memory.align,
                Type::I64,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::I64Store16 { memory } => binaryen::Expression::store(
                into_mod,
                2,
                memory.offset,
                memory.align,
                Type::I64,
                args[0],
                args[1],
                memory.memory,
            ),
            Operator::I64Store32 { memory } => binaryen::Expression::store(
                into_mod,
                4,
                memory.offset,
                memory.align,
                Type::I64,
                args[0],
                args[1],
                memory.memory,
            ),

            Operator::I32Const { value } => binaryen::Expression::const_i32(into_mod, value),
            Operator::I64Const { value } => binaryen::Expression::const_i64(into_mod, value),
            Operator::F32Const { value } => binaryen::Expression::const_f32(into_mod, value),
            Operator::F64Const { value } => binaryen::Expression::const_f64(into_mod, value),

            Operator::I32Eqz => todo!(),
            Operator::I32Eq => todo!(),
            Operator::I32Ne => todo!(),
            Operator::I32LtS => todo!(),
            Operator::I32LtU => todo!(),
            Operator::I32GtS => todo!(),
            Operator::I32GtU => todo!(),
            Operator::I32LeS => todo!(),
            Operator::I32LeU => todo!(),
            Operator::I32GeS => todo!(),
            Operator::I32GeU => todo!(),

            Operator::I64Eqz => todo!(),

            Operator::I64Eq => todo!(),
            Operator::I64Ne => todo!(),
            Operator::I64LtS => todo!(),
            Operator::I64LtU => todo!(),
            Operator::I64GtU => todo!(),
            Operator::I64GtS => todo!(),
            Operator::I64LeS => todo!(),
            Operator::I64LeU => todo!(),
            Operator::I64GeS => todo!(),
            Operator::I64GeU => todo!(),

            Operator::F32Eq => todo!(),
            Operator::F32Ne => todo!(),
            Operator::F32Lt => todo!(),
            Operator::F32Gt => todo!(),
            Operator::F32Le => todo!(),
            Operator::F32Ge => todo!(),

            Operator::F64Eq => todo!(),
            Operator::F64Ne => todo!(),
            Operator::F64Lt => todo!(),
            Operator::F64Gt => todo!(),
            Operator::F64Le => todo!(),
            Operator::F64Ge => todo!(),

            Operator::I32Clz => todo!(),
            Operator::I32Ctz => todo!(),
            Operator::I32Popcnt => todo!(),

            Operator::I32Add => todo!(),
            Operator::I32Sub => todo!(),
            Operator::I32Mul => todo!(),
            Operator::I32DivS => todo!(),
            Operator::I32DivU => todo!(),
            Operator::I32RemS => todo!(),
            Operator::I32RemU => todo!(),
            Operator::I32And => todo!(),
            Operator::I32Or => todo!(),
            Operator::I32Xor => todo!(),
            Operator::I32Shl => todo!(),
            Operator::I32ShrS => todo!(),
            Operator::I32ShrU => todo!(),
            Operator::I32Rotl => todo!(),
            Operator::I32Rotr => todo!(),

            Operator::I64Clz => todo!(),
            Operator::I64Ctz => todo!(),
            Operator::I64Popcnt => todo!(),

            Operator::I64Add => todo!(),
            Operator::I64Sub => todo!(),
            Operator::I64Mul => todo!(),
            Operator::I64DivS => todo!(),
            Operator::I64DivU => todo!(),
            Operator::I64RemS => todo!(),
            Operator::I64RemU => todo!(),
            Operator::I64And => todo!(),
            Operator::I64Or => todo!(),
            Operator::I64Xor => todo!(),
            Operator::I64Shl => todo!(),
            Operator::I64ShrS => todo!(),
            Operator::I64ShrU => todo!(),
            Operator::I64Rotl => todo!(),
            Operator::I64Rotr => todo!(),

            Operator::F32Abs => todo!(),
            Operator::F32Neg => todo!(),
            Operator::F32Ceil => todo!(),
            Operator::F32Floor => todo!(),
            Operator::F32Trunc => todo!(),
            Operator::F32Nearest => todo!(),
            Operator::F32Sqrt => todo!(),

            Operator::F32Add => todo!(),
            Operator::F32Sub => todo!(),
            Operator::F32Mul => todo!(),
            Operator::F32Div => todo!(),
            Operator::F32Min => todo!(),
            Operator::F32Max => todo!(),
            Operator::F32Copysign => todo!(),

            Operator::F64Abs => todo!(),
            Operator::F64Neg => todo!(),
            Operator::F64Ceil => todo!(),
            Operator::F64Floor => todo!(),
            Operator::F64Trunc => todo!(),
            Operator::F64Nearest => todo!(),
            Operator::F64Sqrt => todo!(),

            Operator::F64Add => todo!(),
            Operator::F64Sub => todo!(),
            Operator::F64Mul => todo!(),
            Operator::F64Div => todo!(),
            Operator::F64Min => todo!(),
            Operator::F64Max => todo!(),
            Operator::F64Copysign => todo!(),

            Operator::I32WrapI64 => todo!(),
            Operator::I32TruncF32S => todo!(),
            Operator::I32TruncF32U => todo!(),
            Operator::I32TruncF64S => todo!(),
            Operator::I32TruncF64U => todo!(),
            Operator::I64ExtendI32S => todo!(),
            Operator::I64ExtendI32U => todo!(),
            Operator::I64TruncF32S => todo!(),
            Operator::I64TruncF32U => todo!(),
            Operator::I64TruncF64S => todo!(),
            Operator::I64TruncF64U => todo!(),
            Operator::F32ConvertI32S => todo!(),
            Operator::F32ConvertI32U => todo!(),
            Operator::F32ConvertI64S => todo!(),
            Operator::F32ConvertI64U => todo!(),
            Operator::F32DemoteF64 => todo!(),
            Operator::F64ConvertI32S => todo!(),
            Operator::F64ConvertI32U => todo!(),
            Operator::F64ConvertI64S => todo!(),
            Operator::F64ConvertI64U => todo!(),
            Operator::F64PromoteF32 => todo!(),
            Operator::I32Extend8S => todo!(),
            Operator::I32Extend16S => todo!(),
            Operator::I64Extend8S => todo!(),
            Operator::I64Extend16S => todo!(),
            Operator::I64Extend32S => todo!(),
            Operator::I32TruncSatF32S => todo!(),
            Operator::I32TruncSatF32U => todo!(),
            Operator::I32TruncSatF64S => todo!(),
            Operator::I32TruncSatF64U => todo!(),
            Operator::I64TruncSatF32S => todo!(),
            Operator::I64TruncSatF32U => todo!(),
            Operator::I64TruncSatF64S => todo!(),
            Operator::I64TruncSatF64U => todo!(),
            Operator::F32ReinterpretI32 => todo!(),
            Operator::F64ReinterpretI64 => todo!(),
            Operator::I32ReinterpretF32 => todo!(),
            Operator::I64ReinterpretF64 => todo!(),
            Operator::TableGet { table_index } => todo!(),
            Operator::TableSet { table_index } => todo!(),
            Operator::TableGrow { table_index } => todo!(),
            Operator::TableSize { table_index } => todo!(),
            Operator::MemorySize { mem } => todo!(),
            Operator::MemoryGrow { mem } => todo!(),
        }
    }

    fn local_ty(&self, local: Local) -> Type {
        self.body
            .locals
            .get(local)
            .copied()
            .unwrap_or_else(|| self.new_locals[local.index() - self.body.locals.len()])
    }
}

pub(crate) fn create_new_func(
    module: &Module,
    sig: Signature,
    body: &FunctionBody,
    into_mod: &mut binaryen::Module,
    body_expr: binaryen::Expression,
    new_locals: Vec<Type>,
) {
    // Create param types.
    let sig = module.signature(sig);
    let _func = binaryen::Function::create(
        into_mod,
        sig.params.iter().copied(),
        sig.returns.iter().copied(),
        body.locals
            .values()
            .copied()
            .skip(body.n_params)
            .chain(new_locals.into_iter()),
        body_expr,
    );
}
