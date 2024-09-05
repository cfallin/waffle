//! Lightweight CFG analyses.

// Borrowed from regalloc2's cfg.rs, which is also Apache-2.0 with
// LLVM exception.

use crate::declare_entity;
use crate::entity::{EntityRef, EntityVec, PerEntity};
use crate::ir::{Block, FunctionBody, Terminator, Value, ValueDef};
use smallvec::SmallVec;

pub mod domtree;
pub mod postorder;

declare_entity!(RPOIndex, "rpo");

/// Auxiliary analyses of the control-flow graph.
#[derive(Clone, Debug)]
pub struct CFGInfo {
    /// Entry block.
    pub entry: Block,
    /// Blocks that end in return.
    pub return_blocks: Vec<Block>,
    /// Reverse-postorder traversal of blocks.
    pub rpo: EntityVec<RPOIndex, Block>,
    /// Position of each block in RPO, if reachable.
    pub rpo_pos: PerEntity<Block, Option<RPOIndex>>,
    /// Domtree parents, indexed by block.
    pub domtree: PerEntity<Block, Block>,
    /// Domtree children.
    pub domtree_children: PerEntity<Block, DomtreeChildren>,
    /// Defining block for a given value.
    pub def_block: PerEntity<Value, Block>,
    /// Preds for a given block.
    pub preds: PerEntity<Block, SmallVec<[Block; 4]>>,
    /// A given block's position in each predecessor's successor list.
    pub pred_pos: PerEntity<Block, SmallVec<[usize; 4]>>,
}

#[derive(Clone, Debug, Default)]
pub struct DomtreeChildren {
    pub child: Block,
    pub next: Block,
}

pub struct DomtreeChildIter<'a> {
    domtree_children: &'a PerEntity<Block, DomtreeChildren>,
    block: Block,
}

impl<'a> Iterator for DomtreeChildIter<'a> {
    type Item = Block;
    fn next(&mut self) -> Option<Block> {
        if self.block.is_invalid() {
            None
        } else {
            let block = self.block;
            self.block = self.domtree_children[block].next;
            Some(block)
        }
    }
}

impl CFGInfo {
    pub fn new(f: &FunctionBody) -> CFGInfo {
        let mut return_blocks = vec![];
        let mut preds: PerEntity<Block, SmallVec<[Block; 4]>> = PerEntity::default();
        let mut pred_pos: PerEntity<Block, SmallVec<[usize; 4]>> = PerEntity::default();
        for (block_id, block) in f.blocks.entries() {
            if let Terminator::Return { .. } = &block.terminator {
                return_blocks.push(block_id);
            }
            let mut target_idx = 0;
            block.terminator.visit_targets(|target| {
                preds[target.block].push(block_id);
                pred_pos[target.block].push(target_idx);
                target_idx += 1;
            });
        }

        let postorder = postorder::calculate(f.entry, |block| &f.blocks[block].succs[..]);

        let domtree =
            domtree::calculate(|block| &f.blocks[block].preds[..], &postorder[..], f.entry);

        let mut domtree_children: PerEntity<Block, DomtreeChildren> = PerEntity::default();
        for block in f.blocks.iter().rev() {
            let idom = domtree[block];
            if idom.is_valid() {
                let next = domtree_children[idom].child;
                domtree_children[block].next = next;
                domtree_children[idom].child = block;
            }
        }

        let mut def_block: PerEntity<Value, Block> = PerEntity::default();
        for (block, block_def) in f.blocks.entries() {
            for &(_, param) in &block_def.params {
                def_block[param] = block;
            }
            for &value in &block_def.insts {
                def_block[value] = block;
            }
        }
        for value in f.values.iter() {
            let orig_value = f.resolve_alias(value);
            let underlying_value = match &f.values[orig_value] {
                &ValueDef::PickOutput(value, ..) => value,
                _ => orig_value,
            };
            def_block[value] = def_block[underlying_value];
        }

        let mut rpo = postorder;
        rpo.reverse();
        let rpo = EntityVec::from(rpo);
        let mut rpo_pos = PerEntity::default();
        for (rpo, &block) in rpo.entries() {
            rpo_pos[block] = Some(rpo);
        }

        CFGInfo {
            entry: f.entry,
            return_blocks,
            rpo,
            rpo_pos,
            domtree,
            domtree_children,
            def_block,
            preds,
            pred_pos,
        }
    }

    pub fn dominates(&self, a: Block, b: Block) -> bool {
        domtree::dominates(&self.domtree, a, b)
    }

    pub fn dom_children<'a>(&'a self, block: Block) -> DomtreeChildIter<'a> {
        DomtreeChildIter {
            domtree_children: &self.domtree_children,
            block: self.domtree_children[block].child,
        }
    }
}
