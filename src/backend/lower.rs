use crate::backend::binaryen;
use crate::ir::*;
use fxhash::FxHashMap;
use wasmparser::Type;

/// Creates a body expression for a function. Returns that expression,
/// and new locals (as their types) that were created as temporaries
/// and need to be appended to `body.locals`.
pub(crate) fn generate_body(
    body: &FunctionBody,
    into_mod: &mut binaryen::Module,
) -> (Vec<Type>, binaryen::Expression) {
    // For each block, generate an expr.
    let mut block_exprs: FxHashMap<BlockId, binaryen::Expression> = FxHashMap::default();
    let mut ctx = ElabCtx::new(body, into_mod);
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

#[derive(Clone, Debug)]
struct ElabCtx {
    value_to_expr: FxHashMap<Value, binaryen::Expression>,
    block_params: FxHashMap<(Block, usize), LocalId>,
    new_locals: Vec<Type>,
}

impl ElabCtx {
    fn new(body: &FunctionBody, into_mod: &mut binaryen::Module) -> ElabCtx {
        // Create locals for each blockparam.
        let mut block_params = FxHashMap::default();
        let mut next_local = body.locals.len() as LocalId;
        let mut new_locals = vec![];
        for block in body.blocks() {
            for &(ty, param) in &body[block].params {
                let new_local = next_local;
                next_local += 1;
                block_params.insert((ty, param), new_local);
                new_locals.push(ty);
            }
        }

        ElabCtx {
            value_to_expr: FxHashMap::default(),
            block_params,
            new_locals,
        }
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
    new_locals: Vec<Type>,
) {
    // Create param types.
    let sig = module.signature(sig);
    let _func = binaryen::Function::create(
        into_mod,
        sig.params.iter().copied(),
        sig.returns.iter().copied(),
        body.locals
            .iter()
            .copied()
            .skip(body.n_params)
            .chain(new_locals.into_iter()),
        body_expr,
    );
}
