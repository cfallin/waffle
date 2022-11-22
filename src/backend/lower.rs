use crate::backend::binaryen;
use crate::entity::EntityRef;
use crate::ir::*;
use crate::ir::{ExportKind, FuncDecl, ImportKind};
use crate::Operator;
use anyhow::Result;
use fxhash::FxHashMap;
use std::collections::BTreeMap;

pub(crate) fn lower(module: &Module) -> Result<binaryen::Module> {
    let mut into_mod = binaryen::Module::new()?;

    log::debug!("creating new module");

    // Create imported tables and globals.
    for import in module.imports() {
        log::debug!("adding import: {:?}", import);
        match &import.kind {
            &ImportKind::Table(table) => {
                into_mod.add_table_import(table, &import.module[..], &import.name[..]);
            }
            &ImportKind::Global(global) => {
                let globdata = module.global(global);
                into_mod.add_global_import(
                    global,
                    &import.module[..],
                    &import.name[..],
                    globdata.ty,
                    globdata.mutable,
                );
            }
            _ => {}
        }
    }

    // Create globals.
    for (global, data) in module.globals() {
        log::debug!("adding global {}: {:?}", global, data);
        let new_global = into_mod.add_global(data.ty, data.mutable, data.value);
        assert_eq!(new_global, global);
    }

    // Create tables.
    for (table, data) in module.tables() {
        log::debug!("adding table {}: {:?}", table, data);
        let new_table = into_mod.add_table(
            data.ty,
            data.func_elements
                .as_ref()
                .map(|elems| elems.len())
                .unwrap_or(0),
            data.max,
        );
        assert_eq!(new_table, table);
    }

    // Create memories.
    for (mem, data) in module.memories() {
        log::debug!("adding mem {}", mem);
        let new_mem = into_mod.add_mem(data.initial_pages, data.maximum_pages, &data.segments[..]);
        assert_eq!(new_mem, mem);
    }

    // Create function imports.
    for import in module.imports() {
        log::debug!("adding import: {:?}", import);
        match &import.kind {
            &ImportKind::Func(func) => {
                let sig = module.func(func).sig();
                let sigdata = module.signature(sig);
                into_mod.add_func_import(
                    func,
                    &import.module[..],
                    &import.name[..],
                    &sigdata.params[..],
                    &sigdata.returns[..],
                );
            }
            _ => {}
        }
    }

    // Create function bodies.
    for (func, decl) in module.funcs() {
        log::debug!("adding func {}: {:?}", func, decl);
        match decl {
            &FuncDecl::Body(sig, ref body) => {
                let (new_locals, body_expr) = generate_body(module, body, &mut into_mod);
                let _func =
                    create_new_func(module, sig, body, &mut into_mod, body_expr, new_locals);
            }
            _ => {}
        }
    }

    // Create table contents.
    for (table, data) in module.tables() {
        if let Some(elts) = data.func_elements.as_ref() {
            log::debug!("adding elts to table {}: {:?}", table, elts);
            for (i, &elt) in elts.iter().enumerate() {
                if elt.is_valid() {
                    into_mod.add_table_elem(table, i, elt);
                }
            }
        }
    }

    // Create exports.
    for export in module.exports() {
        log::debug!("adding export {:?}", export);
        match &export.kind {
            &ExportKind::Table(table) => {
                into_mod.add_table_export(table, &export.name[..]);
            }
            &ExportKind::Func(func) => {
                into_mod.add_func_export(func, &export.name[..]);
            }
            &ExportKind::Global(global) => {
                into_mod.add_global_export(global, &export.name[..]);
            }
            &ExportKind::Memory(memory) => {
                into_mod.add_memory_export(memory, &export.name[..]);
            }
        }
    }

    Ok(into_mod)
}

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
            Operator::Unreachable => binaryen::Expression::unreachable(into_mod),
            Operator::Nop => binaryen::Expression::nop(into_mod),

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

            Operator::I32Eqz => binaryen::Expression::i32_eqz(into_mod, args[0]),
            Operator::I32Eq => binaryen::Expression::i32_eq(into_mod, args[0], args[1]),
            Operator::I32Ne => binaryen::Expression::i32_ne(into_mod, args[0], args[1]),
            Operator::I32LtS => binaryen::Expression::i32_lt_s(into_mod, args[0], args[1]),
            Operator::I32LtU => binaryen::Expression::i32_lt_u(into_mod, args[0], args[1]),
            Operator::I32GtS => binaryen::Expression::i32_gt_s(into_mod, args[0], args[1]),
            Operator::I32GtU => binaryen::Expression::i32_gt_u(into_mod, args[0], args[1]),
            Operator::I32LeS => binaryen::Expression::i32_le_s(into_mod, args[0], args[1]),
            Operator::I32LeU => binaryen::Expression::i32_le_u(into_mod, args[0], args[1]),
            Operator::I32GeS => binaryen::Expression::i32_ge_s(into_mod, args[0], args[1]),
            Operator::I32GeU => binaryen::Expression::i32_ge_u(into_mod, args[0], args[1]),

            Operator::I64Eqz => binaryen::Expression::i64_eqz(into_mod, args[0]),
            Operator::I64Eq => binaryen::Expression::i64_eq(into_mod, args[0], args[1]),
            Operator::I64Ne => binaryen::Expression::i64_ne(into_mod, args[0], args[1]),
            Operator::I64LtS => binaryen::Expression::i64_lt_s(into_mod, args[0], args[1]),
            Operator::I64LtU => binaryen::Expression::i64_lt_u(into_mod, args[0], args[1]),
            Operator::I64GtS => binaryen::Expression::i64_gt_s(into_mod, args[0], args[1]),
            Operator::I64GtU => binaryen::Expression::i64_gt_u(into_mod, args[0], args[1]),
            Operator::I64LeS => binaryen::Expression::i64_le_s(into_mod, args[0], args[1]),
            Operator::I64LeU => binaryen::Expression::i64_le_u(into_mod, args[0], args[1]),
            Operator::I64GeS => binaryen::Expression::i64_ge_s(into_mod, args[0], args[1]),
            Operator::I64GeU => binaryen::Expression::i64_ge_u(into_mod, args[0], args[1]),

            Operator::F32Eq => binaryen::Expression::f32_eq(into_mod, args[0], args[1]),
            Operator::F32Ne => binaryen::Expression::f32_ne(into_mod, args[0], args[1]),
            Operator::F32Lt => binaryen::Expression::f32_lt(into_mod, args[0], args[1]),
            Operator::F32Gt => binaryen::Expression::f32_gt(into_mod, args[0], args[1]),
            Operator::F32Le => binaryen::Expression::f32_le(into_mod, args[0], args[1]),
            Operator::F32Ge => binaryen::Expression::f32_ge(into_mod, args[0], args[1]),

            Operator::F64Eq => binaryen::Expression::f64_eq(into_mod, args[0], args[1]),
            Operator::F64Ne => binaryen::Expression::f64_ne(into_mod, args[0], args[1]),
            Operator::F64Lt => binaryen::Expression::f64_lt(into_mod, args[0], args[1]),
            Operator::F64Gt => binaryen::Expression::f64_gt(into_mod, args[0], args[1]),
            Operator::F64Le => binaryen::Expression::f64_le(into_mod, args[0], args[1]),
            Operator::F64Ge => binaryen::Expression::f64_ge(into_mod, args[0], args[1]),

            Operator::I32Clz => binaryen::Expression::i32_clz(into_mod, args[0]),
            Operator::I32Ctz => binaryen::Expression::i32_ctz(into_mod, args[0]),
            Operator::I32Popcnt => binaryen::Expression::i32_popcnt(into_mod, args[0]),

            Operator::I32Add => binaryen::Expression::i32_add(into_mod, args[0], args[1]),
            Operator::I32Sub => binaryen::Expression::i32_sub(into_mod, args[0], args[1]),
            Operator::I32Mul => binaryen::Expression::i32_mul(into_mod, args[0], args[1]),
            Operator::I32DivS => binaryen::Expression::i32_div_s(into_mod, args[0], args[1]),
            Operator::I32DivU => binaryen::Expression::i32_div_u(into_mod, args[0], args[1]),
            Operator::I32RemS => binaryen::Expression::i32_rem_s(into_mod, args[0], args[1]),
            Operator::I32RemU => binaryen::Expression::i32_rem_u(into_mod, args[0], args[1]),
            Operator::I32And => binaryen::Expression::i32_and(into_mod, args[0], args[1]),
            Operator::I32Or => binaryen::Expression::i32_or(into_mod, args[0], args[1]),
            Operator::I32Xor => binaryen::Expression::i32_xor(into_mod, args[0], args[1]),
            Operator::I32Shl => binaryen::Expression::i32_shl(into_mod, args[0], args[1]),
            Operator::I32ShrS => binaryen::Expression::i32_shr_s(into_mod, args[0], args[1]),
            Operator::I32ShrU => binaryen::Expression::i32_shr_u(into_mod, args[0], args[1]),
            Operator::I32Rotl => binaryen::Expression::i32_rotl(into_mod, args[0], args[1]),
            Operator::I32Rotr => binaryen::Expression::i32_rotr(into_mod, args[0], args[1]),

            Operator::I64Clz => binaryen::Expression::i64_clz(into_mod, args[0]),
            Operator::I64Ctz => binaryen::Expression::i64_ctz(into_mod, args[0]),
            Operator::I64Popcnt => binaryen::Expression::i64_popcnt(into_mod, args[0]),

            Operator::I64Add => binaryen::Expression::i64_add(into_mod, args[0], args[1]),
            Operator::I64Sub => binaryen::Expression::i64_sub(into_mod, args[0], args[1]),
            Operator::I64Mul => binaryen::Expression::i64_mul(into_mod, args[0], args[1]),
            Operator::I64DivS => binaryen::Expression::i64_div_s(into_mod, args[0], args[1]),
            Operator::I64DivU => binaryen::Expression::i64_div_u(into_mod, args[0], args[1]),
            Operator::I64RemS => binaryen::Expression::i64_rem_s(into_mod, args[0], args[1]),
            Operator::I64RemU => binaryen::Expression::i64_rem_u(into_mod, args[0], args[1]),
            Operator::I64And => binaryen::Expression::i64_and(into_mod, args[0], args[1]),
            Operator::I64Or => binaryen::Expression::i64_or(into_mod, args[0], args[1]),
            Operator::I64Xor => binaryen::Expression::i64_xor(into_mod, args[0], args[1]),
            Operator::I64Shl => binaryen::Expression::i64_shl(into_mod, args[0], args[1]),
            Operator::I64ShrS => binaryen::Expression::i64_shr_s(into_mod, args[0], args[1]),
            Operator::I64ShrU => binaryen::Expression::i64_shr_u(into_mod, args[0], args[1]),
            Operator::I64Rotl => binaryen::Expression::i64_rotl(into_mod, args[0], args[1]),
            Operator::I64Rotr => binaryen::Expression::i64_rotr(into_mod, args[0], args[1]),

            Operator::F32Abs => binaryen::Expression::f32_abs(into_mod, args[0]),
            Operator::F32Neg => binaryen::Expression::f32_neg(into_mod, args[0]),
            Operator::F32Ceil => binaryen::Expression::f32_ceil(into_mod, args[0]),
            Operator::F32Floor => binaryen::Expression::f32_floor(into_mod, args[0]),
            Operator::F32Trunc => binaryen::Expression::f32_trunc(into_mod, args[0]),
            Operator::F32Nearest => binaryen::Expression::f32_nearest(into_mod, args[0]),
            Operator::F32Sqrt => binaryen::Expression::f32_sqrt(into_mod, args[0]),

            Operator::F32Add => binaryen::Expression::f32_add(into_mod, args[0], args[1]),
            Operator::F32Sub => binaryen::Expression::f32_sub(into_mod, args[0], args[1]),
            Operator::F32Mul => binaryen::Expression::f32_mul(into_mod, args[0], args[1]),
            Operator::F32Div => binaryen::Expression::f32_div(into_mod, args[0], args[1]),
            Operator::F32Min => binaryen::Expression::f32_min(into_mod, args[0], args[1]),
            Operator::F32Max => binaryen::Expression::f32_max(into_mod, args[0], args[1]),
            Operator::F32Copysign => binaryen::Expression::f32_copysign(into_mod, args[0], args[1]),

            Operator::F64Abs => binaryen::Expression::f64_abs(into_mod, args[0]),
            Operator::F64Neg => binaryen::Expression::f64_neg(into_mod, args[0]),
            Operator::F64Ceil => binaryen::Expression::f64_ceil(into_mod, args[0]),
            Operator::F64Floor => binaryen::Expression::f64_floor(into_mod, args[0]),
            Operator::F64Trunc => binaryen::Expression::f64_trunc(into_mod, args[0]),
            Operator::F64Nearest => binaryen::Expression::f64_nearest(into_mod, args[0]),
            Operator::F64Sqrt => binaryen::Expression::f64_sqrt(into_mod, args[0]),

            Operator::F64Add => binaryen::Expression::f64_add(into_mod, args[0], args[1]),
            Operator::F64Sub => binaryen::Expression::f64_sub(into_mod, args[0], args[1]),
            Operator::F64Mul => binaryen::Expression::f64_mul(into_mod, args[0], args[1]),
            Operator::F64Div => binaryen::Expression::f64_div(into_mod, args[0], args[1]),
            Operator::F64Min => binaryen::Expression::f64_min(into_mod, args[0], args[1]),
            Operator::F64Max => binaryen::Expression::f64_max(into_mod, args[0], args[1]),
            Operator::F64Copysign => binaryen::Expression::f64_copysign(into_mod, args[0], args[1]),

            Operator::I32WrapI64 => binaryen::Expression::i32_wrap_i64(into_mod, args[0]),
            Operator::I32TruncF32S => binaryen::Expression::i32_trunc_f32_s(into_mod, args[0]),
            Operator::I32TruncF32U => binaryen::Expression::i32_trunc_f32_u(into_mod, args[0]),
            Operator::I32TruncF64S => binaryen::Expression::i32_trunc_f64_s(into_mod, args[0]),
            Operator::I32TruncF64U => binaryen::Expression::i32_trunc_f64_u(into_mod, args[0]),
            Operator::I64ExtendI32S => binaryen::Expression::i64_extend_i32_s(into_mod, args[0]),
            Operator::I64ExtendI32U => binaryen::Expression::i64_extend_i32_u(into_mod, args[0]),
            Operator::I64TruncF32S => binaryen::Expression::i64_trunc_f32_s(into_mod, args[0]),
            Operator::I64TruncF32U => binaryen::Expression::i64_trunc_f32_u(into_mod, args[0]),
            Operator::I64TruncF64S => binaryen::Expression::i64_trunc_f64_s(into_mod, args[0]),
            Operator::I64TruncF64U => binaryen::Expression::i64_trunc_f64_u(into_mod, args[0]),
            Operator::F32ConvertI32S => binaryen::Expression::f32_convert_i32_s(into_mod, args[0]),
            Operator::F32ConvertI32U => binaryen::Expression::f32_convert_i32_u(into_mod, args[0]),
            Operator::F32ConvertI64S => binaryen::Expression::f32_convert_i64_s(into_mod, args[0]),
            Operator::F32ConvertI64U => binaryen::Expression::f32_convert_i64_u(into_mod, args[0]),
            Operator::F32DemoteF64 => binaryen::Expression::f32_demote_f64(into_mod, args[0]),
            Operator::F64ConvertI32S => binaryen::Expression::f64_convert_i32_s(into_mod, args[0]),
            Operator::F64ConvertI32U => binaryen::Expression::f64_convert_i32_u(into_mod, args[0]),
            Operator::F64ConvertI64S => binaryen::Expression::f64_convert_i64_s(into_mod, args[0]),
            Operator::F64ConvertI64U => binaryen::Expression::f64_convert_i64_u(into_mod, args[0]),
            Operator::F64PromoteF32 => binaryen::Expression::f64_promote_f32(into_mod, args[0]),
            Operator::I32Extend8S => binaryen::Expression::i32_extend_8_s(into_mod, args[0]),
            Operator::I32Extend16S => binaryen::Expression::i32_extend_16_s(into_mod, args[0]),
            Operator::I64Extend8S => binaryen::Expression::i64_extend_8_s(into_mod, args[0]),
            Operator::I64Extend16S => binaryen::Expression::i64_extend_16_s(into_mod, args[0]),
            Operator::I64Extend32S => binaryen::Expression::i64_extend_32_s(into_mod, args[0]),
            Operator::I32TruncSatF32S => {
                binaryen::Expression::i32_trunc_sat_f32_s(into_mod, args[0])
            }
            Operator::I32TruncSatF32U => {
                binaryen::Expression::i32_trunc_sat_f32_u(into_mod, args[0])
            }
            Operator::I32TruncSatF64S => {
                binaryen::Expression::i32_trunc_sat_f64_s(into_mod, args[0])
            }
            Operator::I32TruncSatF64U => {
                binaryen::Expression::i32_trunc_sat_f64_u(into_mod, args[0])
            }
            Operator::I64TruncSatF32S => {
                binaryen::Expression::i64_trunc_sat_f32_s(into_mod, args[0])
            }
            Operator::I64TruncSatF32U => {
                binaryen::Expression::i64_trunc_sat_f32_u(into_mod, args[0])
            }
            Operator::I64TruncSatF64S => {
                binaryen::Expression::i64_trunc_sat_f64_s(into_mod, args[0])
            }
            Operator::I64TruncSatF64U => {
                binaryen::Expression::i64_trunc_sat_f64_u(into_mod, args[0])
            }
            Operator::F32ReinterpretI32 => {
                binaryen::Expression::f32_reinterpret_i32(into_mod, args[0])
            }
            Operator::F64ReinterpretI64 => {
                binaryen::Expression::f64_reinterpret_i64(into_mod, args[0])
            }
            Operator::I32ReinterpretF32 => {
                binaryen::Expression::i32_reinterpret_f32(into_mod, args[0])
            }
            Operator::I64ReinterpretF64 => {
                binaryen::Expression::i64_reinterpret_f64(into_mod, args[0])
            }
            Operator::TableGet { table_index } => {
                binaryen::Expression::table_get(into_mod, table_index, args[0], tys[0])
            }
            Operator::TableSet { table_index } => {
                binaryen::Expression::table_set(into_mod, table_index, args[0], args[1])
            }
            Operator::TableGrow { table_index } => {
                binaryen::Expression::table_grow(into_mod, table_index, args[0], args[1])
            }
            Operator::TableSize { table_index } => {
                binaryen::Expression::table_size(into_mod, table_index)
            }
            Operator::MemorySize { mem } => binaryen::Expression::memory_size(into_mod, mem),
            Operator::MemoryGrow { mem } => {
                binaryen::Expression::memory_grow(into_mod, mem, args[0])
            }
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
