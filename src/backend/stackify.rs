//! Stackifier-like algorithm to recover (or create) structured
//! control flow out of a CFG.

use crate::{cfg::CFGInfo, ir::*};

#[derive(Clone, Debug)]
pub enum Shape {
    Block { head: BlockId, children: Vec<Shape> },
    Loop { head: BlockId, children: Vec<Shape> },
    Leaf { block: BlockId },
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

impl Shape {
    /// Finds the next shape in the sequence, returning the shape and
    /// the remaining starting block ID / region list.
    fn get_one_shape<'a>(
        start: OrderedBlockId,
        order: &[BlockId],
        regions: &'a [Region],
    ) -> Option<(Shape, OrderedBlockId, &'a [Region])> {
        log::trace!("get_one_shape: start {} regions {:?}", start, regions);
        if start >= order.len() {
            None
        } else if regions.is_empty() || start < regions[0].start().block {
            Some((
                Shape::Leaf {
                    block: order[start],
                },
                start + 1,
                &regions,
            ))
        } else {
            assert_eq!(start, regions[0].start().block);
            let end = regions[0].end();
            let region_end = regions
                .iter()
                .position(|region| region.start() > end)
                .unwrap_or(regions.len());
            let subregions = &regions[1..region_end];
            let (children, next_start) = Self::get_shapes(start, end.block, order, subregions);
            let shape = if let Region::Forward(..) = &regions[0] {
                Shape::Block {
                    head: order[start],
                    children,
                }
            } else {
                Shape::Loop {
                    head: order[start],
                    children,
                }
            };
            Some((shape, next_start, &regions[region_end..]))
        }
    }

    fn get_shapes<'a>(
        start: OrderedBlockId,
        end: OrderedBlockId,
        order: &[BlockId],
        mut regions: &'a [Region],
    ) -> (Vec<Shape>, OrderedBlockId) {
        log::trace!("get_shapes: start {} regions {:?}", start, regions);
        let mut shapes = vec![];
        let mut block = start;
        while block < end {
            let (shape, next_start, next_regions) =
                Self::get_one_shape(block, order, regions).unwrap();
            shapes.push(shape);
            block = next_start;
            regions = next_regions;
        }
        (shapes, block)
    }
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
        log::trace!("f = {:?}", f);
        log::trace!("cfg = {:?}", cfg);
        let order = cfg.rpo();
        log::trace!("rpo = {:?}", order);

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
        log::trace!("regions = {:?}", regions);

        // Examine each region in the sequence, determining whether it
        // is properly nested with respect to all overlapping regions.
        let mut i = 0;
        while i < regions.len() {
            let this_i = i;
            for prev_i in 0..i {
                let prev = regions[prev_i];
                let this = regions[i];
                log::trace!("examining: {:?} -> {:?}", prev, this);

                if !prev.overlaps(&this) {
                    log::trace!(" -> no overlap");
                    continue;
                }

                // Important invariant: none of these
                // merging/extension operations alter the sorted
                // order, because at worst they "pull back" the start
                // of the second region (`this`) to the start of the
                // first (`prev`). If the list was sorted by
                // region-start before, it will be after this edit.
                let did_edit = match (prev, this) {
                    (a, b) if a == b => {
                        regions.remove(i);
                        true
                    }
                    (Region::Backward(a, b), Region::Backward(c, d)) if a == c => {
                        // Merge by extending end.
                        regions[prev_i] = Region::Backward(a, std::cmp::max(b, d));
                        regions.remove(i);
                        true
                    }
                    (Region::Backward(a, b), Region::Backward(c, d))
                        if a < c && c <= b && b < d =>
                    {
                        // Extend outer Backward to nest the inner one.
                        regions[prev_i] = Region::Backward(a, d);
                        true
                    }
                    (Region::Backward(a, b), Region::Forward(c, d)) if a <= c && c <= b => {
                        // Put the Forward before the Backward (extend its
                        // start) to ensure proper nesting.
                        regions[prev_i] = Region::Forward(a, d);
                        regions.remove(i);
                        regions.insert(prev_i + 1, Region::Backward(a, b));
                        true
                    }
                    (Region::Forward(a, b), Region::Backward(c, d)) if b > c && b <= d && a < c => {
                        panic!("Irreducible CFG");
                    }
                    (Region::Forward(a, b), Region::Forward(c, d)) if b == d => {
                        // Merge.
                        regions[prev_i] = Region::Forward(std::cmp::min(a, c), b);
                        regions.remove(i);
                        true
                    }
                    (Region::Forward(a, b), Region::Forward(c, d)) if a <= c && b < d => {
                        regions[prev_i] = Region::Forward(a, d);
                        regions.remove(i);
                        regions.insert(prev_i + 1, Region::Forward(a, b));
                        true
                    }
                    _ => false,
                };

                if did_edit {
                    // Back up to re-examine at prev_i.
                    i = prev_i;
                    break;
                }
            }

            if i == this_i {
                i += 1;
            }
        }

        log::trace!("after stackifying: {:?}", regions);

        // Ensure the regions properly nest.
        #[cfg(debug_assertions)]
        {
            let mut stack: Vec<Region> = vec![];
            for region in &regions {
                log::trace!("checking region nest: {:?} (stack = {:?})", region, stack);
                while let Some(top) = stack.last() {
                    log::trace!(" -> top = {:?}", top);
                    if top.contains(region) {
                        stack.push(region.clone());
                        log::trace!(" -> push");
                        break;
                    } else if region.overlaps(top) {
                        panic!(
                            "Non-nested region: {:?} (overlaps {:?}) in nest: {:?} (overall: {:?})",
                            region, top, stack, regions
                        );
                    } else {
                        log::trace!(" -> pop");
                        stack.pop();
                    }
                }
                if stack.is_empty() {
                    stack.push(region.clone());
                }
            }
        }

        // TODO: make regions properly nest by doing replication right
        // as we compute the RPO. Track the current nesting, and
        // traverse more than once if needed.

        // Build the final shape description.
        let (shapes, _) = Shape::get_shapes(0, order.len(), &order[..], &regions[..]);
        let root = if shapes.len() == 1 {
            shapes.into_iter().next().unwrap()
        } else {
            Shape::Block {
                head: 0,
                children: shapes,
            }
        };
        log::trace!("shape: {:?}", root);
        root
    }
}
