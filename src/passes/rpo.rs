//! Reorder-into-RPO pass.
//!
//! The RPO sort order we choose is quite special: we want loop bodies
//! to be placed contiguously, without blocks that do not belong to
//! the loop in the middle.
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
//! but we really would rather not do that, since we may not otherwise
//! need it.
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
    fn prev(self) -> RPOIndex {
        RPOIndex::from(self.0.checked_sub(1).unwrap())
    }
}

#[derive(Clone, Debug, Default)]
struct RPO {
    order: EntityVec<RPOIndex, Block>,
    rev: PerEntity<Block, Option<RPOIndex>>,
}

impl RPO {
    fn compute(body: &FunctionBody) -> RPO {
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
        let order = EntityVec::from(postorder);

        let mut rev = PerEntity::default();
        for (rpo_index, &block) in order.entries() {
            rev[block] = Some(rpo_index);
        }

        RPO { order, rev }
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
        pending.extend(body.blocks[block].succs.iter().copied());

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
        // those seqs not yet present.
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

    fn map_block(&self, block: Block) -> Block {
        Block::new(self.rev[block].unwrap().index())
    }
}

pub fn run(body: &mut FunctionBody) {
    let rpo = RPO::compute(body);
    // Remap entry block.
    body.entry = rpo.map_block(body.entry);
    // Reorder blocks.
    let mut block_data = std::mem::take(&mut body.blocks).into_vec();
    let mut new_block_data = vec![];
    for block in rpo.order.values().copied() {
        new_block_data.push(std::mem::take(&mut block_data[block.index()]));
    }
    body.blocks = EntityVec::from(new_block_data);
    // Rewrite references in each terminator, pred and succ list.
    for block in body.blocks.values_mut() {
        block.terminator.update_targets(|target| {
            target.block = rpo.map_block(target.block);
        });
        for pred in &mut block.preds {
            *pred = rpo.map_block(*pred);
        }
        for succ in &mut block.succs {
            *succ = rpo.map_block(*succ);
        }
    }
}
