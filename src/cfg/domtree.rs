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

use crate::ir::{BlockId, INVALID_BLOCK};

// Helper
fn merge_sets(
    idom: &[BlockId], // map from BlockId to BlockId
    block_to_rpo: &[Option<u32>],
    mut node1: BlockId,
    mut node2: BlockId,
) -> BlockId {
    while node1 != node2 {
        if node1 == INVALID_BLOCK || node2 == INVALID_BLOCK {
            return INVALID_BLOCK;
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

pub fn calculate<'a, PredFn: Fn(BlockId) -> &'a [BlockId]>(
    num_blocks: usize,
    preds: PredFn,
    post_ord: &[BlockId],
    start: BlockId,
) -> Vec<BlockId> {
    // We have post_ord, which is the postorder sequence.

    // Compute maps from RPO to block number and vice-versa.
    let mut block_to_rpo = vec![None; num_blocks];
    block_to_rpo.resize(num_blocks, None);
    for (i, rpo_block) in post_ord.iter().rev().enumerate() {
        block_to_rpo[*rpo_block] = Some(i as u32);
    }

    let mut idom = vec![INVALID_BLOCK; num_blocks];

    // The start node must have itself as a parent.
    idom[start] = start;

    let mut changed = true;
    while changed {
        changed = false;
        // Consider blocks in reverse postorder. Skip any that are unreachable.
        for &node in post_ord.iter().rev() {
            let rponum = block_to_rpo[node].unwrap();

            let mut parent = INVALID_BLOCK;
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

            if parent != INVALID_BLOCK {
                for &pred in preds(node).iter() {
                    if pred == parent {
                        continue;
                    }
                    if idom[pred] == INVALID_BLOCK {
                        continue;
                    }
                    parent = merge_sets(&idom, &block_to_rpo[..], parent, pred);
                }
            }

            if parent != INVALID_BLOCK && parent != idom[node] {
                idom[node] = parent;
                changed = true;
            }
        }
    }

    // Now set the start node's dominator-tree parent to "invalid";
    // this allows the loop in `dominates` to terminate.
    idom[start] = INVALID_BLOCK;

    idom
}

pub fn dominates(idom: &[BlockId], a: BlockId, mut b: BlockId) -> bool {
    loop {
        if a == b {
            return true;
        }
        if b == INVALID_BLOCK {
            return false;
        }
        b = idom[b];
    }
}
