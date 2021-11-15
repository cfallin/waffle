//! IR-to-Wasm transform.

use crate::{cfg::CFGInfo, ir::*};

#[derive(Clone, Debug)]
pub enum Shape {
    Block { head: BlockId, children: Vec<Shape> },
    Loop { head: BlockId, children: Vec<Shape> },
    Leaf { block: BlockId, succs: Vec<BlockId> },
}

enum Region {
    /// Forward-branch region. Extends from end (just prior to
    /// terminator) of first block to just before second block. Can be
    /// extended earlier, prior to the beginning, if needed.
    Forward(BlockId, BlockId),

    /// Backward-branch region. Extends from start of first block to
    /// end (after terminator) of second block. Can be extended past
    /// the end if needed. TODO: actually record all jump-points.
    Backward(BlockId, BlockId),
}

impl Shape {
    pub fn compute(f: &FunctionBody, cfg: &CFGInfo) -> Self {
        // Process all non-contiguous edges in RPO block order. For
        // forward and backward edges, emit Regions.

        // Sort regions by start. Then examine adjacent regions to
        // resolve nesting. If out-of-order, we can extend a Forward
        // region's start backward, or a Backward region's end
        // forward. If still out-of-order, drop any conflicting
        // Backward; we'll handle by duplication.

        todo!()
    }
}
