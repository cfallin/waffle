//! Stackifier-like algorithm to recover (or create) structured
//! control flow out of a CFG.

use std::collections::BTreeSet;

use fxhash::{FxHashMap, FxHashSet};

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
            &Region::Forward(a, _) | &Region::Backward(a, _) => RegionEndpoint::end(a),
        }
    }

    fn end(&self) -> RegionEndpoint {
        match self {
            &Region::Forward(_, b) | &Region::Backward(_, b) => RegionEndpoint::start(b),
        }
    }

    fn key(&self) -> RegionEndpoint {
        match self {
            &Region::Forward(..) => self.end(),
            &Region::Backward(..) => self.start(),
        }
    }

    fn contains(&self, other: &Region) -> bool {
        self.start() <= other.start() && self.end() >= other.end()
    }

    fn contains_endpoint(&self, pt: RegionEndpoint) -> bool {
        self.start() <= pt && pt <= self.end()
    }

    fn overlaps(&self, other: &Region) -> bool {
        self.end() >= other.start() && other.end() >= self.start()
    }

    fn is_forward(&self) -> bool {
        match self {
            &Region::Forward(..) => true,
            _ => false,
        }
    }

    fn is_backward(&self) -> bool {
        match self {
            &Region::Backward(..) => true,
            _ => false,
        }
    }

    fn adjust_nesting(&mut self, outer: &mut Region) -> bool {
        let key1 = std::cmp::min(self.key(), outer.key());
        let key2 = std::cmp::max(self.key(), outer.key());
        let self_key = self.key();

        let swapped = self.adjust_nesting_impl(outer);

        assert!(outer.contains(self));
        assert_eq!(key1, std::cmp::min(self.key(), outer.key()));
        assert_eq!(key2, std::cmp::max(self.key(), outer.key()));
        assert!(self_key <= self.key());

        swapped
    }

    /// Returns `true` if regions were swapped.
    fn adjust_nesting_impl(&mut self, outer: &mut Region) -> bool {
        match (outer, self) {
            (
                &mut Region::Forward(ref mut a, ref mut b),
                &mut Region::Forward(ref mut c, ref mut d),
            ) => {
                assert!(*b <= *d); // scan order
                if *c <= *a {
                    std::mem::swap(a, c);
                    std::mem::swap(b, d);
                    true
                } else if *c < *b {
                    *a = *c;
                    std::mem::swap(b, d);
                    true
                } else {
                    false
                }
            }
            (&mut Region::Forward(_, b), &mut Region::Backward(c, _)) => {
                assert!(b <= c); // scan order

                // nothing to do: no overlap possible.
                false
            }
            (outer @ &mut Region::Backward(..), inner @ &mut Region::Forward(..)) => {
                let a = outer.start().block;
                let b = outer.end().block;
                let c = inner.start().block;
                let d = inner.end().block;

                assert!(a <= d); // scan order

                if b < d {
                    let new_outer = Region::Forward(a, d);
                    let new_inner = Region::Backward(a, b);
                    *outer = new_outer;
                    *inner = new_inner;
                    true
                } else {
                    false
                }
            }
            (
                &mut Region::Backward(ref mut a, ref mut b),
                &mut Region::Backward(ref mut c, ref mut d),
            ) => {
                assert!(*a <= *c); // scan order

                if *b < *d {
                    *b = *d;
                    true
                } else {
                    false
                }
            }
        }
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

        // Look for irreducible edges. TODO: handle these by
        // introducing label variables, then editing the region to
        // refer to the canonical header block. Take care when jumping
        // into multiple nested loops.
        let backedge_targets = regions
            .iter()
            .filter(|r| r.is_backward())
            .map(|r| r.start().block)
            .collect::<BTreeSet<_>>();

        for region in &regions {
            if let &Region::Forward(from, to) = region {
                if let Some(&header_block) = backedge_targets.range((from + 1)..to).next() {
                    panic!(
                        "Irreducible edge from block {} to block {}: jumps into loop with header block {}",
                        order[from], order[to], order[header_block]
                    );
                }
            }
        }

        // Sort regions by either their "target": either their start
        // (for backward regions) or end (for forward regions). This
        // will be the final order of the regions; we can extend the
        // "source" (the opposite endpoint) as needed to ensure proper
        // nesting.
        regions.sort_by_key(|r| r.key());
        log::trace!("regions = {:?}", regions);

        // Now scan the regions, tracking the stack as we go; where we
        // encounter a region that overlaps region(s) on the stack,
        // find the largest enclosing region, and adjust the region to
        // enclose it, inserting it in the stack at that point.
        //
        // [    ...)
        //   (...       ]
        //       (...   ]
        //                [    ...)
        //                  [        ...)
        // We scan by "sorting key", which is the branch target; it is
        // the start of backward regions and end of forward regions.
        //
        // We maintain the invariant that `stack` always contains all
        // regions that contain the scan point (at the start of the
        // loop body, up to the previous scan point; after loop body,
        // updated wrt the current scan point).
        let mut stack: Vec<usize> = vec![];
        for i in 0..regions.len() {
            // Pop from the stack any regions that no longer contain the target.
            while let Some(&top_idx) = stack.last() {
                if !regions[top_idx].contains_endpoint(regions[i].key()) {
                    stack.pop();
                } else {
                    break;
                }
            }

            // Push the current region.
            stack.push(i);

            // Go up the stack, extending all applicable regions.
            for i in (0..(stack.len() - 1)).rev() {
                let mut outer = regions[stack[i]];
                let mut inner = regions[stack[i + 1]];
                let swapped = inner.adjust_nesting(&mut outer);
                regions[stack[i]] = outer;
                regions[stack[i + 1]] = inner;
                if swapped {
                    stack.swap(i, i + 1);
                }
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
