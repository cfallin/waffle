//! Pass to remove empty blocks.

use crate::entity::EntityRef;
use crate::ir::{Block, BlockTarget, FunctionBody, Terminator, Value, ValueDef};
use std::borrow::Cow;
use std::collections::HashSet;

#[derive(Clone, Debug)]
struct Forwarding {
    to: Block,
    args: Vec<ForwardingArg>,
}

#[derive(Clone, Copy, Debug)]
enum ForwardingArg {
    BlockParam(usize),
    Value(Value),
}

impl Forwarding {
    fn compose(a: &Forwarding, b: &Forwarding) -> Forwarding {
        // `b` should be the target of `a.to`, but we can't assert
        // that here. The composed target is thus `b.to`.
        let to = b.to;

        // For each arg in `b.args`, evaluate, replacing any
        // `BlockParam` with the corresponding value from `a.args`.
        let args = b
            .args
            .iter()
            .map(|&arg| match arg {
                ForwardingArg::BlockParam(idx) => a.args[idx].clone(),
                ForwardingArg::Value(v) => ForwardingArg::Value(v),
            })
            .collect::<Vec<_>>();

        Forwarding { to, args }
    }
}

fn block_to_forwarding(body: &FunctionBody, block: Block) -> Option<Forwarding> {
    // Must be empty except for terminator, and must have an
    // unconditional-branch terminator.
    if body.blocks[block].insts.len() > 0 {
        return None;
    }
    let target = match &body.blocks[block].terminator {
        &Terminator::Br { ref target } => target,
        _ => return None,
    };

    // If conditions met, then gather ForwardingArgs.
    let args = target
        .args
        .iter()
        .map(|&arg| {
            let arg = body.resolve_alias(arg);
            match &body.values[arg] {
                &ValueDef::BlockParam(param_block, index, _) if param_block == block => {
                    ForwardingArg::BlockParam(index)
                }
                _ => ForwardingArg::Value(arg),
            }
        })
        .collect::<Vec<_>>();

    Some(Forwarding {
        to: target.block,
        args,
    })
}

fn rewrite_target(forwardings: &[Option<Forwarding>], target: &BlockTarget) -> Option<BlockTarget> {
    if !forwardings[target.block.index()].is_some() {
        return None;
    }

    let mut forwarding = Cow::Borrowed(forwardings[target.block.index()].as_ref().unwrap());
    let mut seen = HashSet::new();
    while forwardings[forwarding.to.index()].is_some() && seen.insert(forwarding.to.index()) {
        forwarding = Cow::Owned(Forwarding::compose(
            &forwarding,
            forwardings[forwarding.to.index()].as_ref().unwrap(),
        ));
    }

    let args = forwarding
        .args
        .iter()
        .map(|arg| match arg {
            &ForwardingArg::Value(v) => v,
            &ForwardingArg::BlockParam(idx) => target.args[idx],
        })
        .collect::<Vec<_>>();

    Some(BlockTarget {
        block: forwarding.to,
        args,
    })
}

pub fn run(body: &mut FunctionBody) {
    // Identify empty blocks, and to where they should forward.
    let forwardings = body
        .blocks
        .iter()
        .map(|block| {
            if block != body.entry {
                block_to_forwarding(body, block)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    // Rewrite every target according to a forwarding (or potentially
    // a chain of composed forwardings).
    for block_data in body.blocks.values_mut() {
        block_data.terminator.update_targets(|target| {
            if let Some(new_target) = rewrite_target(&forwardings[..], target) {
                *target = new_target;
            }
        });
    }

    // Recompute preds/succs.
    body.recompute_edges();
}
