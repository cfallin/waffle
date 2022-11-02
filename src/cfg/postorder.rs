//! Fast postorder computation.

// Borrowed from regalloc2's postorder.rs, which is also Apache-2.0
// with LLVM-exception.

use crate::entity::PerEntity;
use crate::ir::Block;
use smallvec::{smallvec, SmallVec};

pub fn calculate<'a, SuccFn: Fn(Block) -> &'a [Block]>(
    entry: Block,
    succ_blocks: SuccFn,
) -> Vec<Block> {
    let mut ret = vec![];

    // State: visited-block map, and explicit DFS stack.
    let mut visited: PerEntity<Block, bool> = PerEntity::default();

    #[derive(Debug)]
    struct State<'a> {
        block: Block,
        succs: &'a [Block],
        next_succ: usize,
    }
    let mut stack: SmallVec<[State; 64]> = smallvec![];

    visited[entry] = true;
    stack.push(State {
        block: entry,
        succs: succ_blocks(entry),
        next_succ: 0,
    });

    while let Some(ref mut state) = stack.last_mut() {
        log::trace!("postorder: TOS is {:?}", state);
        // Perform one action: push to new succ, skip an already-visited succ, or pop.
        if state.next_succ < state.succs.len() {
            let succ = state.succs[state.next_succ];
            log::trace!(" -> succ {}", succ);
            state.next_succ += 1;
            if !visited[succ] {
                log::trace!(" -> visiting");
                visited[succ] = true;
                stack.push(State {
                    block: succ,
                    succs: succ_blocks(succ),
                    next_succ: 0,
                });
            }
        } else {
            log::trace!("retreating from {}", state.block);
            ret.push(state.block);
            stack.pop();
        }
    }

    ret
}
