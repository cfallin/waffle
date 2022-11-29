//! Stackify implementation to produce structured control flow from an
//! arbitrary CFG.
//!
//! See the paper
//!
//! - Norman Ramsey. Beyond Relooper: recursive translation of
//!   unstructured control flow to structured control flow. In ICFP
//!   2022 (Functional Pearl). https://dl.acm.org/doi/10.1145/3547621
//!
//! for more details on how this algorithm works.

use crate::cfg::CFGInfo;
use crate::entity::EntityRef;
use crate::ir::{Block, BlockTarget, FunctionBody, Terminator, Type, Value};
use crate::passes::rpo::RPO;
use std::collections::HashSet;
use std::convert::TryFrom;

#[derive(Clone, Debug)]
pub enum WasmBlock<'a> {
    /// A Wasm block that has the given contents and whose label jumps
    /// to the given CFG block exit.
    Block {
        body: Vec<WasmBlock<'a>>,
        out: Block,
    },
    /// A Wasm loop that has the given contents and whos label jumps
    /// to the given CFG block header.
    Loop {
        body: Vec<WasmBlock<'a>>,
        header: Block,
    },
    /// A leaf node: one CFG block.
    Leaf { block: Block },
    /// A translated unconditional branch.
    Br { target: WasmLabel },
    /// A translated conditional.
    If {
        cond: Value,
        if_true: Vec<WasmBlock<'a>>,
        if_false: Vec<WasmBlock<'a>>,
    },
    /// A translated select (switch).
    Select {
        selector: Value,
        targets: Vec<WasmLabel>,
        default: WasmLabel,
    },
    /// Blockparam transfer.
    BlockParams {
        from: &'a [Value],
        to: &'a [(Type, Value)],
    },
    /// A function return instruction.
    Return { values: &'a [Value] },
    /// An unreachable instruction.
    Unreachable,
}

/// A Wasm branch target label: number of scopes outward to branch to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WasmLabel(u32);
impl WasmLabel {
    fn new(i: usize) -> WasmLabel {
        WasmLabel(u32::try_from(i).unwrap())
    }
    fn add(&self, extra: usize) -> WasmLabel {
        WasmLabel(self.0.checked_add(u32::try_from(extra).unwrap()).unwrap())
    }
}

struct Context<'a> {
    body: &'a FunctionBody,
    cfg: &'a CFGInfo,
    rpo: &'a RPO,
    merge_nodes: HashSet<Block>,
    loop_headers: HashSet<Block>,
    ctrl_stack: Vec<CtrlEntry>,
}

#[derive(Clone, Copy, Debug)]
enum CtrlEntry {
    Block { out: Block },
    Loop { header: Block },
    IfThenElse,
}

impl CtrlEntry {
    fn label(&self) -> Block {
        match self {
            CtrlEntry::Block { out } => *out,
            CtrlEntry::Loop { header } => *header,
            CtrlEntry::IfThenElse => Block::invalid(),
        }
    }
}

impl<'a> Context<'a> {
    fn new(body: &'a FunctionBody, cfg: &'a CFGInfo, rpo: &'a RPO) -> Self {
        let (merge_nodes, loop_headers) =
            Self::compute_merge_nodes_and_loop_headers(body, cfg, rpo);
        Self {
            body,
            cfg,
            rpo,
            merge_nodes,
            loop_headers,
            ctrl_stack: vec![],
        }
    }

    fn compute_merge_nodes_and_loop_headers(
        body: &FunctionBody,
        cfg: &CFGInfo,
        rpo: &RPO,
    ) -> (HashSet<Block>, HashSet<Block>) {
        let mut loop_headers = HashSet::new();
        let mut branched_once = HashSet::new();
        let mut merge_nodes = HashSet::new();

        for (block_rpo, &block) in rpo.order.entries() {
            for &succ in cfg.succs(block) {
                let succ_rpo = rpo.rev[succ].unwrap();
                if succ_rpo <= block_rpo {
                    // Backward branch.
                    loop_headers.insert(succ);
                } else {
                    // Forward branch.
                    if !branched_once.insert(succ) {
                        merge_nodes.insert(succ);
                    }
                }
            }
        }

        // Make any `select` target a "merge node" too, so it gets its
        // own block.
        for &block in rpo.order.values() {
            if let &Terminator::Select {
                ref targets,
                ref default,
                ..
            } = &body.blocks[block].terminator
            {
                for target in &targets[..] {
                    merge_nodes.insert(target.block);
                }
                merge_nodes.insert(default.block);
            }
        }

        (merge_nodes, loop_headers)
    }

    fn handle_dom_subtree(&mut self, block: Block, into: &mut Vec<WasmBlock<'a>>) {
        let mut merge_node_children = self
            .cfg
            .dom_children(block)
            .filter(|child| self.merge_nodes.contains(&child))
            .collect::<Vec<_>>();
        // Sort merge nodes so highest RPO number comes first.
        merge_node_children.sort_unstable_by_key(|&block| self.rpo.rev[block]);

        let is_loop_header = self.loop_headers.contains(&block);
        if is_loop_header {
            self.ctrl_stack.push(CtrlEntry::Loop { header: block });
            let mut body = vec![];
            self.node_within(block, &merge_node_children[..], &mut body);
            self.ctrl_stack.pop();
            into.push(WasmBlock::Loop {
                body,
                header: block,
            });
        } else {
            self.node_within(block, &merge_node_children[..], into);
        }
    }

    fn resolve_target(&self, target: Block) -> WasmLabel {
        WasmLabel(
            u32::try_from(
                self.ctrl_stack
                    .iter()
                    .rev()
                    .position(|frame| frame.label() == target)
                    .expect("Target must be in control stack"),
            )
            .expect("More than 2^32 frames"),
        )
    }

    fn do_branch(&mut self, source: Block, target: &'a BlockTarget, into: &mut Vec<WasmBlock<'a>>) {
        // This will be a branch to some entry in the control stack if
        // the target is either a merge block, or is a backward branch
        // (by RPO number).
        if self.merge_nodes.contains(&target.block)
            || self.rpo.rev[target.block] < self.rpo.rev[source]
        {
            let index = self.resolve_target(target.block);
            self.do_blockparam_transfer(
                &target.args[..],
                &self.body.blocks[target.block].params[..],
                into,
            );
            into.push(WasmBlock::Br { target: index });
        } else {
            // Otherwise, we must dominate the block, so just emit it inline.
            debug_assert!(self.cfg.dominates(source, target.block));
            self.do_blockparam_transfer(
                &target.args[..],
                &self.body.blocks[target.block].params[..],
                into,
            );
            self.handle_dom_subtree(target.block, into);
        }
    }

    fn do_branch_select(
        &self,
        selector: Value,
        targets: &'a [BlockTarget],
        default: &'a BlockTarget,
        into: &mut Vec<WasmBlock<'a>>,
    ) {
        let mut body = vec![WasmBlock::Select {
            selector,
            targets: (0..targets.len())
                .map(|i| WasmLabel::new(i))
                .collect::<Vec<_>>(),
            default: WasmLabel::new(targets.len()),
        }];

        let mut extra = targets.len() + 1;
        for target in targets.iter().chain(std::iter::once(default)) {
            extra -= 1;
            let outer_body = vec![
                WasmBlock::Block {
                    body,
                    out: Block::invalid(),
                },
                WasmBlock::BlockParams {
                    from: &target.args[..],
                    to: &self.body.blocks[target.block].params[..],
                },
                WasmBlock::Br {
                    target: self.resolve_target(target.block).add(extra),
                },
            ];
            body = outer_body;
        }

        into.extend(body.into_iter());
    }

    fn do_blockparam_transfer(
        &self,
        from: &'a [Value],
        to: &'a [(Type, Value)],
        into: &mut Vec<WasmBlock<'a>>,
    ) {
        into.push(WasmBlock::BlockParams { from, to });
    }

    fn node_within(&mut self, block: Block, merge_nodes: &[Block], into: &mut Vec<WasmBlock<'a>>) {
        if let Some((&first, rest)) = merge_nodes.split_first() {
            self.ctrl_stack.push(CtrlEntry::Block { out: first });
            let mut body = vec![];
            self.node_within(block, rest, &mut body);
            into.push(WasmBlock::Block { body, out: first });
            self.ctrl_stack.pop();
            self.handle_dom_subtree(first, into);
        } else {
            into.push(WasmBlock::Leaf { block });
            match &self.body.blocks[block].terminator {
                &Terminator::Br { ref target } => self.do_branch(block, target, into),
                &Terminator::CondBr {
                    cond,
                    ref if_true,
                    ref if_false,
                } => {
                    self.ctrl_stack.push(CtrlEntry::IfThenElse);
                    let mut if_true_body = vec![];
                    self.do_branch(block, if_true, &mut if_true_body);
                    let mut if_false_body = vec![];
                    self.do_branch(block, if_false, &mut if_false_body);
                    self.ctrl_stack.pop();
                    into.push(WasmBlock::If {
                        cond,
                        if_true: if_true_body,
                        if_false: if_false_body,
                    });
                }
                &Terminator::Select {
                    value,
                    ref targets,
                    ref default,
                } => {
                    self.do_branch_select(value, targets, default, into);
                }
                &Terminator::Return { ref values } => {
                    into.push(WasmBlock::Return { values });
                }
                &Terminator::Unreachable | &Terminator::None => {
                    into.push(WasmBlock::Unreachable);
                }
            }
        }
    }
}
