//! Iterative dataflow analysis (forward and backward) using lattice
//! analysis values.

use crate::cfg::CFGInfo;
use crate::ir::*;
use crate::pass::Lattice;
use fxhash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry as HashEntry;
use std::{collections::VecDeque, default::Default};
use wasmparser::Type;

impl<'a> FunctionBody<'a> {
    fn insts(&self) -> impl Iterator<Item = &Inst<'a>> {
        self.blocks.iter().map(|block| block.insts.iter()).flatten()
    }
}

pub trait DataflowFunctions<L: Lattice> {
    fn start_block(&self, _lattice: &mut L, _block: BlockId, _param_types: &[Type]) -> bool {
        false
    }
    fn end_block(
        &self,
        _lattce: &mut L,
        _block: BlockId,
        _next: BlockId,
        _terminator: &Terminator,
    ) -> bool {
        false
    }
    fn instruction<'a>(
        &self,
        _lattice: &mut L,
        _block: BlockId,
        _instid: InstId,
        _inst: &Inst<'a>,
    ) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub struct ForwardDataflow<L: Lattice> {
    block_in: FxHashMap<BlockId, L>,
}

impl<L: Lattice> ForwardDataflow<L> {
    pub fn new<'a, D: DataflowFunctions<L>>(f: &FunctionBody<'a>, d: &D) -> Self {
        let mut analysis = Self {
            block_in: FxHashMap::default(),
        };
        analysis.compute(f, d);
        analysis
    }

    fn compute<'a, D: DataflowFunctions<L>>(&mut self, f: &FunctionBody<'a>, d: &D) {
        let mut workqueue = VecDeque::new();
        let mut workqueue_set = FxHashSet::default();

        workqueue.push_back(0);
        workqueue_set.insert(0);
        while let Some(block) = workqueue.pop_front() {
            workqueue_set.remove(&block);

            let mut value = self
                .block_in
                .entry(block)
                .or_insert_with(|| L::top())
                .clone();

            d.start_block(&mut value, block, &f.blocks[block].params[..]);

            for (instid, inst) in f.blocks[block].insts.iter().enumerate() {
                d.instruction(&mut value, block, instid, inst);
            }

            let succs = f.blocks[block].terminator.successors();
            for (i, &succ) in succs.iter().enumerate() {
                let mut value = if i + 1 < succs.len() {
                    value.clone()
                } else {
                    std::mem::replace(&mut value, L::top())
                };

                d.end_block(&mut value, block, succ, &f.blocks[block].terminator);

                let (succ_in, mut changed) = match self.block_in.entry(succ) {
                    HashEntry::Vacant(v) => (v.insert(L::top()), true),
                    HashEntry::Occupied(o) => (o.into_mut(), false),
                };
                changed |= succ_in.meet_with(&value);

                if changed && !workqueue_set.contains(&succ) {
                    workqueue.push_back(succ);
                    workqueue_set.insert(succ);
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct BackwardDataflow<L: Lattice> {
    block_out: FxHashMap<BlockId, L>,
}

impl<L: Lattice> BackwardDataflow<L> {
    pub fn new<'a, D: DataflowFunctions<L>>(
        f: &FunctionBody<'a>,
        cfginfo: &CFGInfo,
        d: &D,
    ) -> Self {
        let mut analysis = Self {
            block_out: FxHashMap::default(),
        };
        analysis.compute(f, cfginfo, d);
        analysis
    }

    fn compute<'a, D: DataflowFunctions<L>>(
        &mut self,
        f: &FunctionBody<'a>,
        cfginfo: &CFGInfo,
        d: &D,
    ) {
        let mut workqueue = VecDeque::new();
        let mut workqueue_set = FxHashSet::default();

        let returns = f
            .blocks
            .iter()
            .enumerate()
            .filter(|(_, block)| matches!(&block.terminator, &Terminator::Return { .. }))
            .map(|(id, _)| id)
            .collect::<Vec<BlockId>>();

        for ret in returns {
            workqueue.push_back(ret);
            workqueue_set.insert(ret);
        }

        while let Some(block) = workqueue.pop_front() {
            workqueue_set.remove(&block);

            let mut value = self
                .block_out
                .entry(block)
                .or_insert_with(|| L::top())
                .clone();

            for (instid, inst) in f.blocks[block].insts.iter().rev().enumerate() {
                d.instruction(&mut value, block, instid, inst);
            }

            d.start_block(&mut value, block, &f.blocks[block].params[..]);

            let preds = &cfginfo.block_preds[block];
            for (i, pred) in preds.iter().cloned().enumerate() {
                let mut value = if i + 1 < preds.len() {
                    value.clone()
                } else {
                    std::mem::replace(&mut value, L::top())
                };

                d.end_block(&mut value, pred, block, &f.blocks[pred].terminator);

                let (pred_out, mut changed) = match self.block_out.entry(pred) {
                    HashEntry::Vacant(v) => (v.insert(L::top()), true),
                    HashEntry::Occupied(o) => (o.into_mut(), false),
                };
                changed |= pred_out.meet_with(&value);

                if changed && !workqueue_set.contains(&pred) {
                    workqueue.push_back(pred);
                    workqueue_set.insert(pred);
                }
            }
        }
    }
}
