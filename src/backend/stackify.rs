//! Stackifier-like algorithm to recover (or create) structured
//! control flow out of a CFG.

use crate::{cfg::CFGInfo, ir::*};
use log::debug;

#[derive(Clone, Debug)]
pub enum Shape {
    Block { head: BlockId, children: Vec<Shape> },
    Loop { head: BlockId, children: Vec<Shape> },
    Leaf { block: BlockId, succs: Vec<BlockId> },
    None,
}

/// Index in RPO.
type OrderedBlockId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Region {
    /// Forward-branch region. Extends from end (just prior to
    /// terminator) of first block to just before second block. Can be
    /// extended earlier, prior to the beginning, if needed.
    Forward(OrderedBlockId, OrderedBlockId),

    /// Backward-branch region. Extends from start of first block to
    /// end (after terminator) of second block. Can be extended past
    /// the end if needed.
    Backward(OrderedBlockId, OrderedBlockId),
    // TODO: support irreducible CFGs by adding a `BackwardDispatch`
    // region kind whose start is adjusted back to the first loop
    // block. BackwardDispatch is contagious, i.e. converts adjacent
    // Backward region records to BackwardDispatch.
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct RegionEndpoint {
    block: OrderedBlockId,
    pt: BlockPoint,
}

impl RegionEndpoint {
    fn start(block: OrderedBlockId) -> Self {
        RegionEndpoint {
            block,
            pt: BlockPoint::Start,
        }
    }
    fn end(block: OrderedBlockId) -> Self {
        RegionEndpoint {
            block,
            pt: BlockPoint::Start,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum BlockPoint {
    Start,
    End,
}

impl Region {
    fn start(&self) -> RegionEndpoint {
        match self {
            &Region::Forward(a, _) => RegionEndpoint::end(a),
            &Region::Backward(a, _) => RegionEndpoint::start(a),
        }
    }

    fn end(&self) -> RegionEndpoint {
        match self {
            &Region::Forward(_, b) => RegionEndpoint::start(b),
            &Region::Backward(_, b) => RegionEndpoint::end(b),
        }
    }

    fn contains(&self, other: &Region) -> bool {
        self.start() <= other.start() && self.end() >= other.end()
    }

    fn overlaps(&self, other: &Region) -> bool {
        self.end() >= other.start() && other.end() >= self.start()
    }
}

impl Shape {
    pub fn compute(f: &FunctionBody, cfg: &CFGInfo) -> Self {
        // Process all non-contiguous edges in RPO block order. For
        // forward and backward edges, emit Regions.
        debug!("f = {:?}", f);
        debug!("cfg = {:?}", cfg);
        let order = cfg.rpo();
        debug!("rpo = {:?}", order);

        assert_eq!(order[0], 0); // Entry block should come first.
        let mut regions = vec![];
        for (block_pos, &block) in order.iter().enumerate() {
            for &succ in cfg.succs(block) {
                let succ_pos = cfg
                    .rpo_pos(succ)
                    .expect("if block is reachable then succ should be too");
                if succ_pos < block_pos {
                    regions.push(Region::Backward(succ_pos, block_pos));
                } else if succ_pos > block_pos + 1 {
                    regions.push(Region::Forward(block_pos, succ_pos));
                }
            }
        }

        // Initially sort regions by start pos to get them in rough
        // order; we'll resolve exact ordering below by extending
        // regions where necessary and duplicating where we find
        // irreducible control flow.
        regions.sort_by_key(|r| r.start());
        debug!("regions = {:?}", regions);

        // Examine each region in the sequence, determining whether it
        // is properly nested with respect to all overlapping regions.
        let mut i = 0;
        while i + 1 < regions.len() {
            i += 1;
            let prev = regions[i - 1];
            let this = regions[i];
            debug!("examining: {:?} -> {:?}", prev, this);

            if !prev.overlaps(&this) {
                debug!(" -> no overlap");
                continue;
            }

            // Important invariant: none of these merging/extension
            // operations alter the sorted order, because at worst
            // they "pull back" the start of the second region
            // (`this`) to the start of the first (`prev`). If the
            // list was sorted by region-start before, it will be
            // after this edit.
            let did_edit = match (prev, this) {
                (a, b) if a == b => {
                    regions.remove(i);
                    true
                }
                (Region::Backward(a, b), Region::Backward(c, d)) if a == c => {
                    // Merge by extending end.
                    regions[i - 1] = Region::Backward(a, std::cmp::max(b, d));
                    regions.remove(i);
                    true
                }
                (Region::Backward(a, b), Region::Backward(c, d)) if a < c && c <= b && b < d => {
                    // Extend outer Backward to nest the inner one.
                    regions[i - 1] = Region::Backward(a, d);
                    true
                }
                (Region::Backward(a, b), Region::Forward(c, d)) if a <= c && c <= b => {
                    // Put the Forward before the Backward (extend its
                    // start) to ensure proper nesting.
                    regions[i - 1] = Region::Forward(a, d);
                    regions[i] = Region::Backward(a, b);
                    true
                }
                (Region::Forward(a, b), Region::Backward(c, d)) if b > c && b <= d && a < c => {
                    panic!("Irreducible CFG");
                }
                (Region::Forward(a, b), Region::Forward(c, d)) if b == d => {
                    // Merge.
                    regions[i - 1] = Region::Forward(std::cmp::min(a, c), b);
                    regions.remove(i);
                    true
                }
                (Region::Forward(a, b), Region::Forward(c, d)) if a <= c && b < d => {
                    regions[i - 1] = Region::Forward(a, d);
                    regions[i] = Region::Forward(a, b);
                    true
                }
                _ => false,
            };

            if did_edit {
                // Back up to re-examine i-1 vs i-2, unless we
                // were examining i=0 vs i=1 already.
                i = std::cmp::max(2, i) - 2;
            }
        }

        debug!("after stackifying: {:?}", regions);

        // Ensure the regions properly nest.
        let mut stack: Vec<Region> = vec![];
        for region in &regions {
            while let Some(top) = stack.last() {
                if top.contains(region) {
                    stack.push(region.clone());
                    break;
                } else if region.contains(top) {
                    stack.pop();
                } else if region.overlaps(top) {
                    panic!(
                        "Non-nested region: {:?} in nest: {:?} (overall: {:?})",
                        region, stack, regions
                    );
                }
            }
            if stack.is_empty() {
                stack.push(region.clone());
            }
        }

        // TODO: make regions properly nest by doing replication right
        // as we compute the RPO. Track the current nesting, and
        // traverse more than once if needed.

        Shape::None
    }
}
