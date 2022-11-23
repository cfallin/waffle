//! Stackify implementation to produce structured control flow from an
//! arbitrary CFG.
//!
//! Note on algorithm:
//!
//! - We sort in RPO, then mark loops, then place blocks within loops
//!   or at top level to give forward edges appropriate targets.
//!
//!  - The RPO sort order we choose is quite special: we need loop
//!    bodies to be placed contiguously, without blocks that do not
//!    belong to the loop in the middle. Otherwise we may not be able
//!    to properly nest a block to allow a forward edge.
//!
//! Consider the following CFG:
//!
//! ```plain
//!           1
//!           |
//!           2 <-.
//!         / |   |
//!        |  3 --'
//!        |  |
//!        `> 4
//!           |
//!           5
//! ```
//!
//! A normal RPO sort may produce 1, 2, 4, 5, 3 or 1, 2, 3, 4, 5
//! depending on which child order it chooses from block 2. (If it
//! visits 3 first, it will emit it first in postorder hence it comes
//! last.)
//!
//! One way of ensuring we get the right order would be to compute the
//! loop nest and make note of loops when choosing children to visit,
//! but we really would rather not do that, since we don't otherwise
//! have the infrastructure to compute that or the need for it.
//!
//! Instead, we keep a "pending" list: as we have nodes on the stack
//! during postorder traversal, we keep a list of other children that
//! we will visit once we get back to a given level. If another node
//! is pending, and is a successor we are considering, we visit it
//! *first* in postorder, so it is last in RPO. This is a way to
//! ensure that (e.g.) block 4 above is visited first when considering
//! successors of block 2.

use crate::declare_entity;
use crate::entity::{EntityRef, EntityVec, PerEntity};
use crate::ir::{Block, FunctionBody};
use std::collections::{HashMap, HashSet};

declare_entity!(RPOIndex, "rpo");

impl RPOIndex {
    pub fn prev(self) -> RPOIndex {
        RPOIndex(self.0.checked_sub(1).unwrap())
    }
}

pub struct RPO {
    pub order: EntityVec<RPOIndex, Block>,
    pub rev: PerEntity<Block, RPOIndex>,
}

impl RPO {
    pub fn compute(body: &FunctionBody) -> RPO {
        let mut postorder = vec![];
        let mut visited = HashSet::new();
        let mut pending = vec![];
        let mut pending_idx = HashMap::new();
        visited.insert(body.entry);
        Self::visit(
            body,
            body.entry,
            &mut visited,
            &mut pending,
            &mut pending_idx,
            &mut postorder,
        );
        postorder.reverse();

        let mut rev = PerEntity::default();
        for (i, block) in postorder.iter().copied().enumerate() {
            rev[block] = RPOIndex(i as u32);
        }

        RPO {
            order: EntityVec::from(postorder),
            rev,
        }
    }

    fn visit(
        body: &FunctionBody,
        block: Block,
        visited: &mut HashSet<Block>,
        pending: &mut Vec<Block>,
        pending_idx: &mut HashMap<Block, usize>,
        postorder: &mut Vec<Block>,
    ) {
        // `pending` is a Vec, not a Set; we prioritize based on
        // position (first in pending go first in postorder -> last in
        // RPO). A case with nested loops to show why this matters:
        //
        // TODO example

        let pending_top = pending.len();
        pending.extend(body.blocks[block].succs.clone());

        // Sort new entries in `pending` by index at which they appear
        // earlier. Those that don't appear in `pending` at all should
        // be visited last (to appear in RPO first), so we want `None`
        // values to sort first here (hence the "unwrap or MAX"
        // idiom).  Then those that appear earlier in `pending` should
        // be visited earlier here to appear later in RPO, so they
        // sort later.
        pending[pending_top..]
            .sort_by_key(|entry| pending_idx.get(entry).copied().unwrap_or(usize::MAX));

        // Above we placed items in order they are to be visited;
        // below we pop off the end, so we reverse here.
        pending[pending_top..].reverse();

        // Now update indices in `pending_idx`: insert entries for
        // those blocks not yet present.
        for i in pending_top..pending.len() {
            pending_idx.entry(pending[i]).or_insert(i);
        }

        for _ in 0..(pending.len() - pending_top) {
            let succ = pending.pop().unwrap();
            if pending_idx.get(&succ) == Some(&pending.len()) {
                pending_idx.remove(&succ);
            }

            if visited.insert(succ) {
                Self::visit(body, succ, visited, pending, pending_idx, postorder);
            }
        }
        postorder.push(block);
    }
}

/// Start and end marks for loops.
#[derive(Debug)]
pub struct Marks(HashMap<RPOIndex, Vec<Mark>>);

// Sorting-order note: Loop comes second, so Blocks sort first with
// smaller regions first. Thus, *reverse* sort order places loops
// outermost then larger blocks before smaller blocks.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mark {
    Block { last_inclusive: RPOIndex },
    Loop { last_inclusive: RPOIndex },
}

impl Marks {
    pub fn compute(body: &FunctionBody, rpo: &RPO) -> anyhow::Result<Marks> {
        let mut marks = HashMap::new();

        // Pass 1: Place loop markers.
        let mut loop_end: HashMap<RPOIndex, RPOIndex> = HashMap::new();
        for (rpo_block, &block) in rpo.order.entries() {
            for &succ in &body.blocks[block].succs {
                let rpo_succ = rpo.rev[succ];
                assert!(rpo_succ.is_valid());
                if rpo_succ <= rpo_block {
                    let end = loop_end.entry(rpo_succ).or_insert(RPOIndex::invalid());
                    if end.is_invalid() {
                        *end = rpo_block;
                    } else {
                        // Already-existing loop header. Adjust `end`.
                        *end = std::cmp::max(*end, rpo_block);
                    }
                }
            }
        }

        // Pass 2: properly nest loops by extending the reach of outer
        // loops to fully contain inner loops.
        for rpo_block in rpo.order.iter().rev() {
            if let Some(rpo_loop_end) = loop_end.get(&rpo_block).copied() {
                let mut updated_end = rpo_loop_end;
                for body_block in rpo_block.index()..=rpo_loop_end.index() {
                    let body_block = RPOIndex::new(body_block);
                    if let Some(inner_end) = loop_end.get(&body_block).copied() {
                        updated_end = std::cmp::max(updated_end, inner_end);
                    }
                }
                if updated_end != rpo_loop_end {
                    loop_end.insert(rpo_block, updated_end);
                }
            }
        }

        // Pass 3: compute location of innermost loop for each
        // block.
        let mut innermost_loop: PerEntity<RPOIndex, Option<RPOIndex>> = PerEntity::default();
        let mut loop_stack: Vec<(RPOIndex, RPOIndex)> = vec![];
        for rpo_block in rpo.order.iter() {
            while let Some(innermost) = loop_stack.last() {
                if innermost.1 >= rpo_block {
                    break;
                }
                loop_stack.pop();
            }

            if let Some(rpo_loop_end) = loop_end.get(&rpo_block).copied() {
                loop_stack.push((rpo_block, rpo_loop_end));
            }

            innermost_loop[rpo_block] = loop_stack.last().map(|lp| lp.0);
        }

        // Copy loop-start markers over.
        for (lp, end) in loop_end {
            marks.insert(
                lp,
                vec![Mark::Loop {
                    last_inclusive: end,
                }],
            );
        }

        // Pass 4: place block markers.
        for (rpo_block, &block) in rpo.order.entries() {
            for &succ in &body.blocks[block].succs {
                let rpo_succ = rpo.rev[succ];
                assert!(rpo_succ.is_valid());
                if rpo_succ > rpo_block {
                    // Determine the innermost loop for the target,
                    // and add the block just inside the loop.
                    let block_start = innermost_loop[rpo_succ].unwrap_or(RPOIndex(0));
                    let start_marks = marks.entry(block_start).or_insert_with(|| vec![]);
                    let mark = Mark::Block {
                        last_inclusive: rpo_succ.prev(),
                    };
                    start_marks.push(mark);
                }
            }
        }

        // Sort markers at each block.
        for marklist in marks.values_mut() {
            marklist.sort();
            marklist.dedup();
            marklist.reverse();
        }

        Ok(Marks(marks))
    }
}
