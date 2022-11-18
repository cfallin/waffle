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
    body: &FunctionBody,
    into_mod: &mut binaryen::Module,
) -> (Vec<Type>, binaryen::Expression) {
    let mut ctx = ElabCtx::new(body, into_mod);

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
    body: &'a FunctionBody,
    op_result_locals: FxHashMap<(Value, usize), Local>,
    block_param_locals: FxHashMap<(Block, usize), Local>,
    block_param_next_locals: FxHashMap<(Block, usize), Local>,
    new_locals: Vec<Type>,
}

impl<'a> ElabCtx<'a> {
    fn new(body: &'a FunctionBody, into_mod: &mut binaryen::Module) -> ElabCtx<'a> {
        todo!()
    }

    fn elaborate_value(
        &mut self,
        into_mod: &binaryen::Module,
        value: Value,
    ) -> Option<binaryen::Expression> {
        /*
        let value = self.body.resolve_alias(value);

        match &self.body.values[value] {
            &ValueDef::Operator(op, ref args, ref tys) => {
                // Get expressions for each arg.
                let args = args.iter().map(|&arg| self.get_val_local(arg));
                // Create `get_local` expressions for each arg.
                let binaryen_args = args
                    .map(|arg_local| into_mod.expr_local_get(arg_local, self.local_ty(arg_local)))
                    .collect::<Vec<_>>();
                // Create operator.
                let expr = self.create_binaryen_op(op, binaryen_args, tys);

                // Set local(s) as appropriate.
                if tys.len() == 0 {
                    // Nothing. Create a `drop` expr that wraps the actual operator.
                    Some(into_mod.expr_drop(expr))
                } else if tys.len() == 1 {
                    // Set value directly.
                    let local = self.get_val_local(value);
                    Some(into_mod.expr_local_set(local, expr))
                } else {
                    todo!("support multivalue")
                }
            }
            _ => None,
        }
         */
        todo!()
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
    ) -> binaryen::Expression {
        todo!()
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
