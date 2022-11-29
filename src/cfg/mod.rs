//! Lightweight CFG analyses.

// Borrowed from regalloc2's cfg.rs, which is also Apache-2.0 with
// LLVM exception.

use crate::entity::{EntityRef, PerEntity};
use crate::ir::{Block, FunctionBody, Terminator, Value, ValueDef};
use smallvec::SmallVec;

pub mod domtree;
pub mod postorder;

#[derive(Clone, Debug)]
pub struct CFGInfo {
    /// Entry block.
    pub entry: Block,
    /// Predecessors for each block.
    pub block_preds: PerEntity<Block, SmallVec<[Block; 4]>>,
    /// Successors for each block.
    pub block_succs: PerEntity<Block, SmallVec<[Block; 4]>>,
    /// Blocks that end in return.
    pub return_blocks: Vec<Block>,
    /// Postorder traversal of blocks.
    pub postorder: Vec<Block>,
    /// Position of each block in postorder, if reachable.
    pub postorder_pos: PerEntity<Block, Option<usize>>,
    /// Domtree parents, indexed by block.
    pub domtree: PerEntity<Block, Block>,
    /// Domtree children.
    pub domtree_children: PerEntity<Block, DomtreeChildren>,
    /// Defining block for a given value.
    pub def_block: PerEntity<Value, Block>,
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
        let mut block_preds: PerEntity<Block, SmallVec<[Block; 4]>> = PerEntity::default();
        let mut block_succs: PerEntity<Block, SmallVec<[Block; 4]>> = PerEntity::default();
        for (block, block_def) in f.blocks.entries() {
            block_def.terminator.visit_successors(|succ| {
                block_preds[succ].push(block);
                block_succs[block].push(succ);
            });
        }

        let mut return_blocks = vec![];
        for (block_id, block) in f.blocks.entries() {
            if let Terminator::Return { .. } = &block.terminator {
                return_blocks.push(block_id);
            }
        }

        let postorder = postorder::calculate(f.entry, |block| &block_succs[block]);

        let mut postorder_pos = PerEntity::default();
        for (i, block) in postorder.iter().enumerate() {
            postorder_pos[*block] = Some(i);
        }

        let domtree = domtree::calculate(|block| &&block_preds[block], &postorder[..], f.entry);

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

        CFGInfo {
            entry: f.entry,
            block_preds,
            block_succs,
            return_blocks,
            postorder,
            postorder_pos,
            domtree,
            domtree_children,
            def_block,
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

    pub fn succs(&self, block: Block) -> &[Block] {
        &self.block_succs[block]
    }

    pub fn preds(&self, block: Block) -> &[Block] {
        &self.block_preds[block]
    }

    pub fn pred_count_with_entry(&self, block: Block) -> usize {
        let is_entry = block == self.entry;
        self.preds(block).len() + if is_entry { 1 } else { 0 }
    }

    pub fn succ_count_with_return(&self, block: Block) -> usize {
        let is_return = self.return_blocks.binary_search(&block).is_ok();
        self.succs(block).len() + if is_return { 1 } else { 0 }
    }

    pub fn rpo(&self) -> Vec<Block> {
        self.postorder.iter().cloned().rev().collect()
    }

    pub fn rpo_pos(&self, block: Block) -> Option<usize> {
        self.postorder_pos[block].map(|fwd_pos| self.postorder.len() - 1 - fwd_pos)
    }
}
