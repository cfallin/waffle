use crate::backend::binaryen;
use crate::ir::*;
use fxhash::FxHashMap;

pub(crate) fn generate_body(
    body: &FunctionBody,
    into_mod: &mut binaryen::Module,
) -> binaryen::Expression {
    // For each block, generate an expr.
    let mut block_exprs: FxHashMap<BlockId, binaryen::Expression> = FxHashMap::default();
    let mut ctx = ElabCtx::default();
    for block in body.blocks() {
        let exprs = body[block]
            .insts
            .iter()
            .map(|&inst| {
                let inst = body.resolve_alias(inst);
                elaborate_value(body, into_mod, &mut ctx, inst)
            })
            .collect::<Vec<_>>();
        block_exprs.insert(block, binaryen::Expression::block(into_mod, &exprs[..]));
    }

    todo!()
}

#[derive(Clone, Debug, Default)]
struct ElabCtx {
    value_to_expr: FxHashMap<Value, binaryen::Expression>,
    block_params: FxHashMap<Value, LocalId>,
    args: FxHashMap<Value, LocalId>,
}

impl ElabCtx {
    fn for_func(module: &Module, func: FuncId) -> ElabCtx {
        let sig = module.func(func).sig();
        let sig = module.signature(sig);
        let body = module.func(func).body().unwrap();

        let mut ctx = ElabCtx::default();

        // TODO
    }
}

fn elaborate_value(
    body: &FunctionBody,
    into_mod: &binaryen::Module,
    ctx: &mut ElabCtx,
    value: Value,
) -> binaryen::Expression {
    let value = body.resolve_alias(value);
    if let Some(expr) = ctx.value_to_expr.get(&value) {
        return *expr;
    }

    match &body[value] {
        &ValueDef::BlockParam(block, idx) => {}
        &ValueDef::Arg(idx) => {}
        &ValueDef::PickOutput(value, idx) => {}
        &ValueDef::Operator(op, ref args) => {}

        &ValueDef::Alias(_) => unreachable!(),
        &ValueDef::Placeholder => unreachable!(),
    }

    todo!()
}

pub(crate) fn create_new_func(
    module: &Module,
    sig: SignatureId,
    body: &FunctionBody,
    into_mod: &mut binaryen::Module,
    body_expr: binaryen::Expression,
) {
    // Create param types.
    let sig = module.signature(sig);
    let _func = binaryen::Function::create(
        into_mod,
        sig.params.iter().copied(),
        sig.returns.iter().copied(),
        body.locals.iter().copied(),
        body_expr,
    );
}
