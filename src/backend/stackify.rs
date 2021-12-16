//! Stackifier-like algorithm to recover (or create) structured
//! control flow out of a CFG.

use std::collections::BTreeSet;

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
            log::trace!(" -> leaf");
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
                .position(|region| region.start() >= end)
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
        log::trace!(
            "get_shapes: start {} end {} regions {:?}",
            start,
            end,
            regions
        );
        let mut shapes = vec![];
        let mut block = start;
        while block < end {
            log::trace!("get_shapes: now at {}, regions {:?}", block, regions);
            let (shape, next_start, next_regions) =
                Self::get_one_shape(block, order, regions).unwrap();
            shapes.push(shape);
            block = next_start;
            log::trace!(" -> next_regions = {:?}", next_regions);
            regions = next_regions;
        }
        log::trace!("get_shapes: returning {:?}", shapes);
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

    fn contains(&self, other: &Region) -> bool {
        self.start() <= other.start() && self.end() >= other.end()
    }

    fn contains_endpoint(&self, pt: RegionEndpoint) -> bool {
        self.start() <= pt && pt <= self.end()
    }

    fn overlaps(&self, other: &Region) -> bool {
        self.end() > other.start() && other.end() > self.start()
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

        // Compute nest of loop headers per block.

        // If a given block is a loop header, then
        // `loop_end[block_rpo_index]` will be
        // Some(last_loop_body_rpo_index)`.
        let mut loop_header_to_end: Vec<Option<OrderedBlockId>> = vec![None; order.len()];
        // Record forward edges as tuples of RPO-block-indices for
        // processing below.
        let mut forward_edges: Vec<(OrderedBlockId, OrderedBlockId)> = vec![];

        for (block_pos, &block) in order.iter().enumerate() {
            for &succ in cfg.succs(block) {
                let succ_pos = cfg
                    .rpo_pos(succ)
                    .expect("if block is reachable then succ should be too");
                if succ_pos < block_pos {
                    let end = loop_header_to_end[succ_pos].unwrap_or(block_pos);
                    let end = std::cmp::max(end, block_pos);
                    loop_header_to_end[succ_pos] = Some(end);
                } else if succ_pos > block_pos + 1 {
                    forward_edges.push((block_pos, succ_pos));
                }
            }
        }

        // Extend loop ends to fully nest subloops. Also build loop
        // nest info for each block.
        let mut stack = vec![];
        let mut loop_nest = vec![];
        for block in 0..order.len() {
            while let Some(&(_first, last)) = stack.last() {
                if block > last {
                    stack.pop();
                } else {
                    break;
                }
            }
            if let Some(end) = loop_header_to_end[block] {
                stack.push((block, end));
            }
            for &mut (start, ref mut end) in &mut stack {
                if block > *end {
                    loop_header_to_end[start] = Some(block);
                    *end = block;
                }
            }
            loop_nest.push(stack.clone());
        }

        log::trace!("loop_header_to_end = {:?}", loop_header_to_end);
        log::trace!("loop_nest = {:?}", loop_nest);

        // Look for irreducible edges. TODO: handle these by
        // introducing label variables, then editing the region to
        // refer to the canonical header block. Take care when jumping
        // into multiple nested loops.
        let loop_headers = loop_header_to_end
            .iter()
            .enumerate()
            .filter(|(_, entry)| entry.is_some())
            .map(|(header, _)| header)
            .collect::<BTreeSet<_>>();

        for &(from, to) in &forward_edges {
            if let Some(&header_block) = loop_headers.range((from + 1)..to).next() {
                panic!(
                        "Irreducible edge from block {} to block {}: jumps into loop with header block {}",
                        order[from], order[to], order[header_block]
                    );
            }
        }

        log::trace!("loop_headers = {:?}", loop_headers);
        log::trace!("forward_edges = {:?}", forward_edges);

        // Process forward edges: add "block-start count" to
        // containing scopes, and mark block-end points.
        let mut block_ends = vec![false; order.len()];
        let mut block_end_to_start = vec![None; order.len()];
        let mut block_starts = vec![vec![]; order.len()];
        for &(from, to) in &forward_edges {
            if !block_ends[to] {
                block_ends[to] = true;
            }
            let start = block_end_to_start[to].unwrap_or(from);
            let start = std::cmp::min(start, from);
            block_end_to_start[to] = Some(start);
        }

        for block in 0..order.len() {
            if let Some(start) = block_end_to_start[block] {
                // Examine loop nest of endpoint. Must be a prefix of
                // loop nest of startpoint if control flow is
                // reducible. If start is inside any additional loops,
                // we need to move the startpoint back to the start of
                // those loops.
                let start_loopnest = &loop_nest[start];
                let end_loopnest = &loop_nest[block];
                // Find common prefix of loopnests at start and end
                // points, and put a block start at the top of that
                // loop. In other words, we put the block around the
                // innermost loop that surrounds the whole forward
                // edge (or around the whole body if not). As long as
                // control flow is reducible, this will not result in
                // an edge into a loop.
                let start_idx = start_loopnest
                    .iter()
                    .zip(end_loopnest.iter())
                    .take_while(|(a, b)| a == b)
                    .count();
                let start = if start_idx > 0 {
                    start_loopnest[start_idx - 1].0
                } else {
                    0
                };

                block_starts[start].push(block);
            }
        }

        log::trace!("block_starts = {:?}", block_starts);
        log::trace!("block_end_to_start = {:?}", block_end_to_start);

        // Now generate the region list which we use to produce the "shape".
        let mut regions = vec![];

        for block in 0..order.len() {
            for &block_end in block_starts[block].iter().rev() {
                regions.push(Region::Forward(block, block_end));
            }
            if let Some(loop_end) = loop_header_to_end[block] {
                regions.push(Region::Backward(block, loop_end));
            }
        }

        log::trace!("after stackifying: {:?}", regions);

        // Ensure the regions properly nest.
        #[cfg(debug_assertions)]
        {
            let mut stack: Vec<Region> = vec![];
            for region in &regions {
                while let Some(top) = stack.last() {
                    if top.contains(region) {
                        stack.push(region.clone());
                        break;
                    } else if region.overlaps(top) {
                        panic!(
                            "Non-nested region: {:?} (overlaps {:?}) in nest: {:?} (overall: {:?})",
                            region, top, stack, regions
                        );
                    } else {
                        stack.pop();
                    }
                }
                if stack.is_empty() {
                    stack.push(region.clone());
                }
            }
        }

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
