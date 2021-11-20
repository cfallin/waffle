//! Lightweight CFG analyses.

// Borrowed from regalloc2's cfg.rs, which is also Apache-2.0 with
// LLVM exception.

use crate::ir::{BlockId, FunctionBody, Terminator};
use smallvec::SmallVec;

pub mod domtree;
pub mod postorder;

#[derive(Clone, Debug)]
pub struct CFGInfo {
    /// Predecessors for each block.
    pub block_preds: Vec</* BlockId, */ SmallVec<[BlockId; 4]>>,
    /// Successors for each block.
    pub block_succs: Vec</* BlockId, */ SmallVec<[BlockId; 4]>>,
    /// Blocks that end in return.
    pub return_blocks: Vec<BlockId>,
    /// Postorder traversal of blocks.
    pub postorder: Vec<BlockId>,
    /// Position of each block in postorder, if reachable.
    pub postorder_pos: Vec</* BlockId, */ Option<usize>>,
    /// Domtree parents, indexed by block.
    pub domtree: Vec<BlockId>,
}

impl CFGInfo {
    pub fn new(f: &FunctionBody) -> CFGInfo {
        let mut block_preds = vec![SmallVec::new(); f.blocks.len()];
        let mut block_succs = vec![SmallVec::new(); f.blocks.len()];
        for block in 0..f.blocks.len() {
            for succ in f.blocks[block].successors() {
                block_preds[succ].push(block);
                block_succs[block].push(succ);
            }
        }

        let mut return_blocks = vec![];
        for block in 0..f.blocks.len() {
            if let Terminator::Return { .. } = &f.blocks[block].terminator {
                return_blocks.push(block);
            }
        }

        let postorder = postorder::calculate(f.blocks.len(), 0, |block| &block_succs[block]);

        let mut postorder_pos = vec![None; f.blocks.len()];
        for (i, block) in postorder.iter().enumerate() {
            postorder_pos[*block] = Some(i);
        }

        let domtree = domtree::calculate(
            f.blocks.len(),
            |block| &&block_preds[block],
            &postorder[..],
            0,
        );

        CFGInfo {
            block_preds,
            block_succs,
            return_blocks,
            postorder,
            postorder_pos,
            domtree,
        }
    }

    pub fn dominates(&self, a: BlockId, b: BlockId) -> bool {
        domtree::dominates(&self.domtree[..], a, b)
    }

    pub fn succs(&self, block: BlockId) -> &[BlockId] {
        &self.block_succs[block]
    }

    pub fn preds(&self, block: BlockId) -> &[BlockId] {
        &self.block_preds[block]
    }

    pub fn pred_count_with_entry(&self, block: BlockId) -> usize {
        let is_entry = block == 0;
        self.preds(block).len() + if is_entry { 1 } else { 0 }
    }

    pub fn succ_count_with_return(&self, block: BlockId) -> usize {
        let is_return = self.return_blocks.binary_search(&block).is_ok();
        self.succs(block).len() + if is_return { 1 } else { 0 }
    }

    pub fn rpo(&self) -> Vec<BlockId> {
        self.postorder.iter().cloned().rev().collect()
    }

    pub fn rpo_pos(&self, block: BlockId) -> Option<usize> {
        self.postorder_pos[block].map(|fwd_pos| self.postorder.len() - 1 - fwd_pos)
    }
}
