//! Multiloop lowering: one top-level `multiloop` region per function
//! with one body per chain of CFG blocks. Arbitrary (including
//! irreducible) control flow is expressed directly as branches
//! between bodies, so no reducification or relooping is needed.

use super::{CompileContext, WasmFuncBackend};
use crate::cfg::CFGInfo;
use crate::entity::PerEntity;
use crate::ir::{Block, BlockTarget, FunctionBody, Terminator};
use std::borrow::Cow;
use std::convert::TryFrom;
use wasm_encoder::{BlockType, Instruction};

/// Emission order of the CFG: each body is a chain of blocks where
/// every non-final block ends in an unconditional branch to the next
/// one (its sole predecessor), lowered as fallthrough. Bodies are in
/// reverse postorder; the engine computes its own block layout from
/// the CFG, so the body order only decides which branches to the next
/// body can be omitted.
pub(crate) struct Layout {
    pub bodies: Vec<Vec<Block>>,
    pub body_index: PerEntity<Block, Option<u32>>,
}

impl Layout {
    pub fn compute(body: &FunctionBody, cfg: &CFGInfo) -> Layout {
        let mut chain_next: PerEntity<Block, Option<Block>> = PerEntity::default();
        let mut follower: PerEntity<Block, bool> = PerEntity::default();
        for &b in cfg.rpo.values() {
            if let Terminator::Br { target } = &body.blocks[b].terminator {
                let s = target.block;
                let reachable_preds = cfg.preds[s]
                    .iter()
                    .filter(|&&p| cfg.rpo_pos[p].is_some())
                    .count();
                if s != b && s != cfg.entry && reachable_preds == 1 {
                    chain_next[b] = Some(s);
                    follower[s] = true;
                }
            }
        }

        let mut bodies = vec![];
        let mut body_index: PerEntity<Block, Option<u32>> = PerEntity::default();
        for &b in cfg.rpo.values() {
            if follower[b] {
                continue;
            }
            body_index[b] = Some(u32::try_from(bodies.len()).unwrap());
            let mut chain = vec![b];
            let mut cur = b;
            while let Some(next) = chain_next[cur] {
                chain.push(next);
                cur = next;
            }
            bodies.push(chain);
        }
        debug_assert_eq!(bodies[0][0], cfg.entry);
        Layout { bodies, body_index }
    }

    fn label(&self, target: Block, depth: u32) -> u32 {
        self.body_index[target].expect("branch target must be a multiloop body head") + depth
    }
}

impl<'a> WasmFuncBackend<'a> {
    pub(super) fn lower_multiloop(
        &self,
        ctx: &CompileContext<'_>,
        func: &mut wasm_encoder::Function,
    ) {
        let layout = Layout::compute(&self.body, &self.cfg);
        let n = layout.bodies.len();
        func.instruction(&Instruction::MultiLoop(Cow::Owned(vec![
            BlockType::Empty;
            n
        ])));
        for (i, chain) in layout.bodies.iter().enumerate() {
            func.instruction(&Instruction::Label);
            for (j, &block) in chain.iter().enumerate() {
                self.lower_leaf(ctx, block, func);
                let fallthrough = chain
                    .get(j + 1)
                    .copied()
                    .or_else(|| layout.bodies.get(i + 1).map(|next| next[0]));
                self.lower_multiloop_terminator(ctx, &layout, block, fallthrough, func);
            }
        }
        func.instruction(&Instruction::End);
        if !self.body.rets.is_empty() {
            func.instruction(&Instruction::Unreachable);
        }
    }

    fn transfer_needed(&self, ctx: &CompileContext<'_>, target: &BlockTarget) -> bool {
        self.body.blocks[target.block]
            .params
            .iter()
            .any(|&(_, to)| !ctx.locals.values[to].is_empty())
    }

    fn lower_transfer(
        &self,
        ctx: &CompileContext<'_>,
        target: &BlockTarget,
        func: &mut wasm_encoder::Function,
    ) {
        self.lower_blockparams(
            ctx,
            &target.args[..],
            &self.body.blocks[target.block].params[..],
            func,
        );
    }

    /// Branch to `target` from `depth` nested constructs inside a
    /// body; a top-level branch to the block that is emitted next is
    /// a fallthrough.
    fn lower_body_br(
        &self,
        layout: &Layout,
        target: Block,
        depth: u32,
        fallthrough: Option<Block>,
        func: &mut wasm_encoder::Function,
    ) {
        if depth == 0 && fallthrough == Some(target) {
            return;
        }
        func.instruction(&Instruction::Br(layout.label(target, depth)));
    }

    fn lower_multiloop_terminator(
        &self,
        ctx: &CompileContext<'_>,
        layout: &Layout,
        block: Block,
        fallthrough: Option<Block>,
        func: &mut wasm_encoder::Function,
    ) {
        match &self.body.blocks[block].terminator {
            Terminator::Br { target } => {
                self.lower_transfer(ctx, target, func);
                self.lower_body_br(layout, target.block, 0, fallthrough, func);
            }
            Terminator::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                // Both arms as an `if`/`else`, each ending in its own
                // branch: the shape the structured lowering emits. A
                // `br_if` plus `br` pair gives the engine two critical
                // edges into the target bodies instead of two arm blocks,
                // and its edge blocks and block order then cost measurably
                // more executed branches (richards +11%); with arm blocks
                // the counts match the structured form.
                self.lower_value(ctx, *cond, func);
                func.instruction(&Instruction::If(BlockType::Empty));
                self.lower_transfer(ctx, if_true, func);
                func.instruction(&Instruction::Br(layout.label(if_true.block, 1)));
                func.instruction(&Instruction::Else);
                self.lower_transfer(ctx, if_false, func);
                func.instruction(&Instruction::Br(layout.label(if_false.block, 1)));
                func.instruction(&Instruction::End);
            }
            Terminator::Select {
                value,
                targets,
                default,
            } => {
                let all = || targets.iter().chain(std::iter::once(default));
                if all().all(|t| !self.transfer_needed(ctx, t)) {
                    self.lower_value(ctx, *value, func);
                    func.instruction(&Instruction::BrTable(
                        Cow::Owned(
                            targets
                                .iter()
                                .map(|t| layout.label(t.block, 0))
                                .collect::<Vec<_>>(),
                        ),
                        layout.label(default.block, 0),
                    ));
                } else {
                    let k = u32::try_from(targets.len() + 1).unwrap();
                    for _ in 0..k {
                        func.instruction(&Instruction::Block(BlockType::Empty));
                    }
                    self.lower_value(ctx, *value, func);
                    func.instruction(&Instruction::BrTable(
                        Cow::Owned((0..k - 1).collect::<Vec<_>>()),
                        k - 1,
                    ));
                    for (i, t) in all().enumerate() {
                        func.instruction(&Instruction::End);
                        let depth = k - 1 - u32::try_from(i).unwrap();
                        self.lower_transfer(ctx, t, func);
                        self.lower_body_br(layout, t.block, depth, fallthrough, func);
                    }
                }
            }
            Terminator::Return { values } => {
                for &value in &values[..] {
                    self.lower_value(ctx, value, func);
                }
                func.instruction(&Instruction::Return);
            }
            Terminator::Unreachable | Terminator::None => {
                func.instruction(&Instruction::Unreachable);
            }
        }
    }
}
