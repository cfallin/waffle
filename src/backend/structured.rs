//! Recovery of structured control flow information. Loop nest
//! computation, block order linearization and loop/block region
//! generation.

use fxhash::{FxHashMap, FxHashSet};

use crate::{cfg::CFGInfo, BlockId, FunctionBody, Value};

#[derive(Clone, Debug)]
pub enum Node {
    Leaf(BlockId),
    Loop(BlockId, Vec<Node>),
}

impl Node {
    pub fn header(&self) -> BlockId {
        match self {
            &Node::Leaf(block) => block,
            &Node::Loop(block, ..) => block,
        }
    }
    pub fn is_loop(&self) -> bool {
        match self {
            &Node::Loop(..) => true,
            _ => false,
        }
    }
    pub fn is_leaf(&self) -> bool {
        match self {
            &Node::Leaf(..) => true,
            _ => false,
        }
    }
}

pub struct LoopNest {
    nodes: Vec<Node>,
}

impl LoopNest {
    pub fn compute(cfg: &CFGInfo) -> LoopNest {
        // Find loop backedges: any successor edge from a higher- to
        // lower-numbered block in RPO.
        let mut backedges: Vec<(BlockId, BlockId)> = vec![];
        for (block_rpo, &block) in cfg.postorder.iter().rev().enumerate() {
            for &succ in &cfg.block_succs[block] {
                let succ_po = cfg.postorder_pos[succ]
                    .expect("Edge from reachable to unreachable block is impossible");
                let succ_rpo = cfg.postorder.len() - 1 - succ_po;
                if succ_rpo <= block_rpo {
                    log::trace!("LoopNest compute: backedge from {} to {}", block, succ);
                    backedges.push((block, succ));
                }
            }
        }

        // For each backedge, find the backedge's natural loop and
        // accumulate those blocks into the set of blocks in each loop
        // body.
        let mut loop_bodies: FxHashMap<BlockId, FxHashSet<BlockId>> = FxHashMap::default();
        for &(from, to) in &backedges {
            assert!(
                cfg.dominates(to, from),
                "Irreducible CFG edge from {} to {}",
                from,
                to
            );
            let body = loop_bodies
                .entry(to)
                .or_insert_with(|| FxHashSet::default());
            Self::collect_loop_body(body, to, cfg);
            log::trace!("loop body for header {}: {:?}", to, body);
        }

        // Now build the loop nest.
        let mut nodes = vec![];
        let mut visited = FxHashSet::default();
        for &block in cfg.postorder.iter().rev() {
            if visited.contains(&block) {
                continue;
            }
            if loop_bodies.contains_key(&block) {
                nodes.push(Self::loop_node(cfg, block, &loop_bodies, &mut visited));
            } else {
                nodes.push(Node::Leaf(block));
                visited.insert(block);
            }
        }

        log::trace!("loop nest nodes: {:?}", nodes);
        LoopNest { nodes }
    }

    fn collect_loop_body(blocks: &mut FxHashSet<BlockId>, header: BlockId, cfg: &CFGInfo) {
        let mut workset = vec![header];
        while let Some(block) = workset.pop() {
            for &pred in &cfg.block_preds[block] {
                if blocks.contains(&pred) {
                    continue;
                }
                if cfg.dominates(header, pred) {
                    blocks.insert(pred);
                    workset.push(pred);
                }
            }
        }
    }

    fn loop_node(
        cfg: &CFGInfo,
        header: BlockId,
        loops: &FxHashMap<BlockId, FxHashSet<BlockId>>,
        visited: &mut FxHashSet<BlockId>,
    ) -> Node {
        let mut body_blocks = loops
            .get(&header)
            .unwrap()
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        body_blocks.sort_by_key(|&block| -(cfg.postorder_pos[block].unwrap() as isize));

        let mut body_nodes = vec![];
        for block in body_blocks {
            if visited.contains(&block) {
                continue;
            }
            if block != header && loops.contains_key(&block) {
                body_nodes.push(Self::loop_node(cfg, block, loops, visited));
            } else {
                body_nodes.push(Node::Leaf(block));
                visited.insert(block);
            }
        }

        Node::Loop(header, body_nodes)
    }
}

fn compute_linear_block_pos(cfg: &CFGInfo, nest: &LoopNest) -> Vec<Option<usize>> {
    let mut next = 0;
    let mut positions = vec![None; cfg.len()];
    for node in &nest.nodes {
        compute_linear_block_pos_for_node(node, &mut next, &mut positions);
    }
    positions
}

fn compute_linear_block_pos_for_node(
    node: &Node,
    next: &mut usize,
    positions: &mut Vec<Option<usize>>,
) {
    match node {
        &Node::Loop(_, ref subnodes) => {
            for subnode in subnodes {
                compute_linear_block_pos_for_node(subnode, next, positions);
            }
        }
        &Node::Leaf(block) => {
            let linear_index = *next;
            *next += 1;
            positions[block] = Some(linear_index);
        }
    }
}

fn compute_forward_edge_targets(
    cfg: &CFGInfo,
    linear_block_pos: &[Option<usize>],
) -> FxHashSet<BlockId> {
    let mut ret = FxHashSet::default();
    for block in 0..cfg.len() {
        if linear_block_pos[block].is_none() {
            continue;
        }
        let block_pos = linear_block_pos[block].unwrap();
        for &succ in &cfg.block_succs[block] {
            let succ_pos = linear_block_pos[succ].unwrap();
            if succ_pos > block_pos + 1 {
                ret.insert(succ);
            }
        }
    }
    ret
}

#[derive(Clone, Debug)]
pub enum WasmRegion {
    /// Block starting at the first `BlockId`, with a fallthrough/exit
    /// label at the second `BlockId`.
    Block(BlockId, Option<BlockId>, Vec<WasmRegion>),
    /// Loop with a header at the given `BlockId`.
    Loop(BlockId, Vec<WasmRegion>),
    /// An individual basic block, just included inline (with no
    /// Wasm-level structure).
    Leaf(BlockId),
}

impl WasmRegion {
    pub fn header(&self) -> BlockId {
        match self {
            &WasmRegion::Block(block, ..) => block,
            &WasmRegion::Loop(block, ..) => block,
            &WasmRegion::Leaf(block) => block,
        }
    }

    pub fn compute(cfg: &CFGInfo, loop_nest: &LoopNest) -> WasmRegion {
        assert!(!loop_nest.nodes.is_empty());
        assert!(loop_nest.nodes[0].header() == 0);

        let linear_pos = compute_linear_block_pos(cfg, loop_nest);
        let forward_targets = compute_forward_edge_targets(cfg, &linear_pos);
        log::trace!(
            "WasmRegion::compute: forward_targets = {:?}",
            forward_targets
        );

        // Enclose loop nest in a virtual loop, to handle forward
        // edges in a unified way even outside any loop.
        let top = Self::compute_for_node(
            cfg,
            &forward_targets,
            &Node::Loop(BlockId::MAX, loop_nest.nodes.clone()),
        );
        let subregions = match top {
            WasmRegion::Loop(_, subregions) => subregions,
            _ => unreachable!(),
        };
        let top = WasmRegion::Block(0, None, subregions);

        log::trace!("Wasm region: {:?}", top);
        top
    }

    fn compute_for_node(
        cfg: &CFGInfo,
        forward_targets: &FxHashSet<BlockId>,
        node: &Node,
    ) -> WasmRegion {
        log::trace!("WasmRegion::compute_for_node: node {:?}", node);
        match node {
            &Node::Leaf(block) => {
                log::trace!(" -> leaf {}", block);
                WasmRegion::Leaf(block)
            }
            &Node::Loop(block, ref subnodes) => {
                // Scan subnodes and find forward-edge targets that
                // are at this level of the loop nest.
                let block_targets = subnodes
                    .iter()
                    .map(|n| n.header())
                    .filter(|n| forward_targets.contains(&n))
                    .collect::<FxHashSet<_>>();
                log::trace!(" -> block targets are {:?}", block_targets,);

                let mut subregions: Vec<WasmRegion> = vec![];
                for subnode in subnodes {
                    if subnode.header() != block && block_targets.contains(&subnode.header()) {
                        let subsubregions = std::mem::take(&mut subregions);
                        assert!(!subsubregions.is_empty());
                        let first = subsubregions[0].header();
                        let enclosing_block =
                            WasmRegion::Block(first, Some(subnode.header()), subsubregions);
                        subregions.push(enclosing_block);
                    }

                    let subregion = Self::compute_for_node(cfg, forward_targets, subnode);
                    subregions.push(subregion);
                }

                log::trace!(" -> loop header {} subregions {:?}", block, subregions);
                WasmRegion::Loop(block, subregions)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct BlockOrder {
    pub entries: Vec<BlockOrderEntry>,
}

#[derive(Clone, Debug)]
pub enum BlockOrderEntry {
    StartBlock(
        BlockId,
        Vec<(wasmparser::Type, Value)>,
        Vec<wasmparser::Type>,
    ),
    StartLoop(
        BlockId,
        Vec<(wasmparser::Type, Value)>,
        Vec<wasmparser::Type>,
    ),
    End,
    BasicBlock(BlockId, Vec<BlockOrderTarget>),
}

#[derive(Clone, Debug)]
pub struct BlockOrderTarget {
    pub target: BlockId,
    /// `None` means fallthrough.
    pub relative_branch: Option<usize>,
    pub args: Vec<Value>,
}

impl BlockOrder {
    pub fn compute(f: &FunctionBody, cfg: &CFGInfo, wasm_region: &WasmRegion) -> BlockOrder {
        let mut target_stack = vec![];
        let mut entries = vec![];
        Self::generate_region(f, cfg, &mut target_stack, &mut entries, wasm_region, None);
        log::trace!("entries: {:?}", entries);
        BlockOrder { entries }
    }

    fn generate_region(
        f: &FunctionBody,
        cfg: &CFGInfo,
        target_stack: &mut Vec<BlockId>,
        entries: &mut Vec<BlockOrderEntry>,
        region: &WasmRegion,
        fallthrough: Option<BlockId>,
    ) {
        log::trace!(
            "BlockOrder::generate_region: stack {:?} region {:?} fallthrough {:?}",
            target_stack,
            region,
            fallthrough,
        );
        match region {
            &WasmRegion::Block(header, _, ref subregions, ..)
            | &WasmRegion::Loop(header, ref subregions) => {
                let (target, is_loop) = match region {
                    &WasmRegion::Block(_, out, ..) => {
                        assert!(out.is_some() || target_stack.is_empty());
                        (out, false)
                    }
                    &WasmRegion::Loop(header, ..) => (Some(header), true),
                    _ => unreachable!(),
                };

                if let Some(target) = target {
                    target_stack.push(target);
                }
                let params = f.blocks[header].params.clone();
                let results = match fallthrough {
                    Some(fallthrough) => f.blocks[fallthrough]
                        .params
                        .iter()
                        .map(|(ty, _)| *ty)
                        .collect(),
                    None => vec![],
                };
                if is_loop {
                    entries.push(BlockOrderEntry::StartLoop(header, params, results));
                } else {
                    entries.push(BlockOrderEntry::StartBlock(header, params, results));
                }

                for i in 0..subregions.len() {
                    let subregion = &subregions[i];
                    let fallthrough = if i == subregions.len() - 1 {
                        fallthrough
                    } else {
                        Some(subregions[i + 1].header())
                    };
                    Self::generate_region(f, cfg, target_stack, entries, subregion, fallthrough);
                }

                entries.push(BlockOrderEntry::End);
                if target.is_some() {
                    target_stack.pop();
                }
            }

            &WasmRegion::Leaf(block) => {
                let mut targets = vec![];
                f.blocks[block].terminator.visit_targets(|target| {
                    log::trace!(
                        "BlockOrder::generate_region: looking for succ {} in stack {:?} fallthrough {:?}",
                        target.block,
                        target_stack,
                        fallthrough,
                    );
                    let relative_branch = if Some(target.block) == fallthrough {
                        None
                    } else {
                        let pos = target_stack
                            .iter()
                            .position(|entry| *entry == target.block)
                            .expect("Malformed Wasm structured control flow");
                        Some(target_stack.len() - 1 - pos)
                    };
                    targets.push(BlockOrderTarget {
                        target: target.block,
                        relative_branch,
                        args: target.args.clone(),
                    });
                });
                entries.push(BlockOrderEntry::BasicBlock(block, targets));
            }
        }
        log::trace!("BlockOrder::generate_region: done with region {:?}", region);
    }
}
