/*
 * Derives from the dominator tree implementation in regalloc.rs, which is
 * licensed under the Apache Public License 2.0 with LLVM Exception. See:
 * https://github.com/bytecodealliance/regalloc.rs
 */

// This is an implementation of the algorithm described in
//
//   A Simple, Fast Dominance Algorithm
//   Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
//   Department of Computer Science, Rice University, Houston, Texas, USA
//   TR-06-33870
//   https://www.cs.rice.edu/~keith/EMBED/dom.pdf

use crate::entity::{EntityRef, PerEntity};
use crate::ir::Block;

// Helper
fn merge_sets(
    idom: &PerEntity<Block, Block>, // map from Block to Block
    block_to_rpo: &PerEntity<Block, Option<u32>>,
    mut node1: Block,
    mut node2: Block,
) -> Block {
    while node1 != node2 {
        if node1.is_invalid() || node2.is_invalid() {
            return Block::invalid();
        }
        let rpo1 = block_to_rpo[node1].unwrap();
        let rpo2 = block_to_rpo[node2].unwrap();
        if rpo1 > rpo2 {
            node1 = idom[node1];
        } else if rpo2 > rpo1 {
            node2 = idom[node2];
        }
    }
    assert!(node1 == node2);
    node1
}

pub fn calculate<'a, PredFn: Fn(Block) -> &'a [Block]>(
    preds: PredFn,
    post_ord: &[Block],
    start: Block,
) -> PerEntity<Block, Block> {
    // We have post_ord, which is the postorder sequence.

    // Compute maps from RPO to block number and vice-versa.
    let mut block_to_rpo: PerEntity<Block, Option<u32>> = PerEntity::default();
    for (i, rpo_block) in post_ord.iter().rev().enumerate() {
        block_to_rpo[*rpo_block] = Some(i as u32);
    }

    let mut idom: PerEntity<Block, Block> = PerEntity::default();

    // The start node must have itself as a parent.
    idom[start] = start;

    let mut changed = true;
    while changed {
        changed = false;
        // Consider blocks in reverse postorder. Skip any that are unreachable.
        for &node in post_ord.iter().rev() {
            let rponum = block_to_rpo[node].unwrap();

            let mut parent = Block::invalid();
            for &pred in preds(node).iter() {
                let pred_rpo = match block_to_rpo[pred] {
                    Some(r) => r,
                    None => {
                        // Skip unreachable preds.
                        continue;
                    }
                };
                if pred_rpo < rponum {
                    parent = pred;
                    break;
                }
            }

            if parent != Block::invalid() {
                for &pred in preds(node).iter() {
                    if pred == parent {
                        continue;
                    }
                    if idom[pred] == Block::invalid() {
                        continue;
                    }
                    parent = merge_sets(&idom, &block_to_rpo, parent, pred);
                }
            }

            if parent != Block::invalid() && parent != idom[node] {
                idom[node] = parent;
                changed = true;
            }
        }
    }

    // Now set the start node's dominator-tree parent to "invalid";
    // this allows the loop in `dominates` to terminate.
    idom[start] = Block::invalid();

    idom
}

pub fn dominates(idom: &PerEntity<Block, Block>, a: Block, mut b: Block) -> bool {
    loop {
        if a == b {
            return true;
        }
        if b.is_invalid() {
            return false;
        }
        b = idom[b];
    }
}
