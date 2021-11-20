//! IR-to-Wasm transform.

use crate::{cfg::CFGInfo, ir::*};

#[derive(Clone, Debug)]
pub enum Shape {
    Block { head: BlockId, children: Vec<Shape> },
    Loop { head: BlockId, children: Vec<Shape> },
    Leaf { block: BlockId, succs: Vec<BlockId> },
    None,
}

/// Index in RPO.
type OrderedBlockId = usize;

#[derive(Clone, Copy, Debug)]
enum Region {
    /// Forward-branch region. Extends from end (just prior to
    /// terminator) of first block to just before second block. Can be
    /// extended earlier, prior to the beginning, if needed.
    Forward(OrderedBlockId, OrderedBlockId),

    /// Backward-branch region. Extends from start of first block to
    /// end (after terminator) of second block. Can be extended past
    /// the end if needed.
    Backward(OrderedBlockId, OrderedBlockId),
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
    pub fn compute(_f: &FunctionBody, cfg: &CFGInfo) -> Self {
        // Process all non-contiguous edges in RPO block order. For
        // forward and backward edges, emit Regions.
        let order = cfg.rpo();
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

        // Examine each region in the sequence, determining whether it
        // is properly nested with respect to all overlapping regions.
        let mut i = 0;
        while i + 1 < regions.len() {
            i += 1;
            let prev = regions[i - 1];
            let this = regions[i];

            if !prev.overlaps(&this) {
                continue;
            }

            match (prev, this) {
                (Region::Backward(a, b), Region::Backward(c, d)) if a == c => {
                    // Merge by extending end.
                    regions[i - 1] = Region::Backward(a, std::cmp::max(b, d));
                    regions.remove(i);
                }
                (Region::Backward(a, b), Region::Backward(c, _d)) if a < c && c <= b => {
                    panic!("Irreducible CFG");
                }
                (Region::Backward(a, b), Region::Forward(c, d)) if a <= c && c <= b => {
                    // Put the Forward before the Backward (extend its
                    // start) to ensure proper nesting.
                    regions[i - 1] = Region::Forward(a, d);
                    regions[i] = Region::Backward(a, b);
                }
                (Region::Forward(_a, b), Region::Backward(c, d)) if b > c && b <= d => {
                    panic!("Irreducible CFG");
                }
                (Region::Forward(a, b), Region::Forward(c, d)) if b == d => {
                    // Merge.
                    regions[i - 1] = Region::Forward(std::cmp::min(a, c), b);
                    regions.remove(i);
                }
                (Region::Forward(a, b), Region::Forward(c, d)) if a <= c && b < d => {
                    regions[i - 1] = Region::Forward(a, d);
                    regions[i] = Region::Forward(a, b);
                }
                _ => {}
            }
        }

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

        Shape::None
    }
}
