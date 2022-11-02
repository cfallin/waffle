/*
use crate::backend::binaryen;
use crate::entity::EntityRef;
use crate::ir::*;
use crate::Operator;
use fxhash::FxHashMap;
use wasmparser::Type;

/// Creates a body expression for a function. Returns that expression,
/// and new locals (as their types) that were created as temporaries
/// and need to be appended to `body.locals`.
pub(crate) fn generate_body(
    body: &FunctionBody,
    into_mod: &mut binaryen::Module,
) -> (Vec<Type>, binaryen::Expression) {
    let mut ctx = ElabCtx::new(body, into_mod);

    // For each block, generate an expr.
    let mut block_exprs: FxHashMap<Block, binaryen::Expression> = FxHashMap::default();
    for (block_id, block) in body.blocks.entries() {
        let exprs = block
            .insts
            .iter()
            .flat_map(|&inst| {
                let inst = body.resolve_alias(inst);
                ctx.elaborate_value(into_mod, inst)
            })
            .collect::<Vec<binaryen::Expression>>();
        block_exprs.insert(block_id, binaryen::Expression::block(into_mod, &exprs[..]));
    }

    // Combine blocks into a single body expression, using the
    // relooper/stackifier support built into Binaryen.
    let mut relooper = binaryen::Relooper::new(into_mod);
    let mut entry = None;
    let mut relooper_blocks: FxHashMap<Block, binaryen::RelooperBlock> = FxHashMap::default();
    for (block_id, block_expr) in block_exprs {}

    let index_var = ctx.new_local(Type::I32);
    let expr = relooper.construct(entry.unwrap(), index_var.index());
    (ctx.new_locals, expr)
}

#[derive(Clone, Debug)]
struct ElabCtx<'a> {
    body: &'a FunctionBody,
    op_result_locals: FxHashMap<(Value, usize), Local>,
    block_param_locals: FxHashMap<(Block, usize), Local>,
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
            &ValueDef::Arg(idx, _) => Local::new(idx),
            &ValueDef::BlockParam(block, idx, _) => {
                self.block_param_locals.get(&(block, idx)).copied().unwrap()
            }
            &ValueDef::Operator(..) => self.op_result_locals.get(&(value, 0)).copied().unwrap(),
            &ValueDef::PickOutput(value, idx, _) => {
                self.op_result_locals.get(&(value, idx)).copied().unwrap()
            }
            &ValueDef::Alias(val) => self.get_val_local(val),
            &ValueDef::Placeholder(_) => unreachable!(),
        }
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
*/
