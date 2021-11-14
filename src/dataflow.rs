//! Dataflow analysis.

use fxhash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use std::{fmt::Debug, hash::Hash};

use crate::{BlockId, FunctionBody, InstId};

pub trait Lattice: Clone + Debug + PartialEq + Eq {
    fn top() -> Self;
    fn bottom() -> Self;
    fn meet(a: &Self, b: &Self) -> Self;
}

pub trait AnalysisKey: Clone + Debug + PartialEq + Eq + Hash {}

impl AnalysisKey for u32 {}

#[derive(Clone, Debug)]
pub struct AnalysisValue<K: AnalysisKey, L: Lattice> {
    pub values: FxHashMap<K, L>,
}

impl<K: AnalysisKey, L: Lattice> std::default::Default for AnalysisValue<K, L> {
    fn default() -> Self {
        Self {
            values: FxHashMap::default(),
        }
    }
}

impl<K: AnalysisKey, L: Lattice> AnalysisValue<K, L> {
    fn meet_with(&mut self, other: &Self, meet_mode: MapMeetMode) -> bool {
        let mut changed = false;
        let mut to_remove = vec![];
        for (key, value) in &mut self.values {
            if let Some(other_value) = other.values.get(key) {
                let met = L::meet(value, other_value);
                if met != *value {
                    changed = true;
                    *value = met;
                }
            } else {
                if meet_mode == MapMeetMode::Intersection {
                    to_remove.push(key.clone());
                    changed = true;
                }
            }
        }
        for k in to_remove {
            self.values.remove(&k);
        }
        if meet_mode == MapMeetMode::Union {
            for (key, value) in &other.values {
                if !self.values.contains_key(key) {
                    self.values.insert(key.clone(), value.clone());
                    changed = true;
                }
            }
        }

        changed
    }
}

pub trait AnalysisFunction {
    type K: AnalysisKey;
    type L: Lattice;

    fn instruction(
        &self,
        _input: &mut AnalysisValue<Self::K, Self::L>,
        _func: &FunctionBody,
        _block: BlockId,
        _inst: InstId,
    ) -> bool {
        false
    }

    fn terminator(
        &self,
        _input: &mut AnalysisValue<Self::K, Self::L>,
        _func: &FunctionBody,
        _block: BlockId,
        _index: usize,
        _next: BlockId,
    ) -> bool {
        false
    }

    fn meet_mode(&self) -> MapMeetMode {
        MapMeetMode::Union
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MapMeetMode {
    Union,
    Intersection,
}

#[derive(Clone, Debug, Default)]
pub struct ForwardDataflow<F: AnalysisFunction> {
    block_in: Vec<AnalysisValue<F::K, F::L>>,
    workqueue: VecDeque<BlockId>,
    workqueue_set: FxHashSet<BlockId>,
}

impl<F: AnalysisFunction> ForwardDataflow<F> {
    pub fn new(analysis: &F, func: &FunctionBody) -> Self {
        let mut ret = ForwardDataflow {
            block_in: vec![AnalysisValue::default(); func.blocks.len()],
            workqueue: vec![0].into(),
            workqueue_set: vec![0].into_iter().collect(),
        };
        ret.compute(analysis, func);
        ret
    }

    fn compute(&mut self, analysis: &F, func: &FunctionBody) {
        while let Some(block) = self.workqueue.pop_front() {
            self.workqueue_set.remove(&block);
            self.update_block(analysis, func, block);
        }
    }

    fn update_block(&mut self, analysis: &F, func: &FunctionBody, block: BlockId) {
        let mut value = self.block_in[block].clone();
        let mut changed = false;
        for i in 0..func.blocks[block].insts.len() {
            changed |= analysis.instruction(&mut value, func, block, i);
        }

        for (i, succ) in func.blocks[block]
            .terminator
            .successors()
            .into_iter()
            .enumerate()
        {
            let mut term_changed = changed;
            let mut value = value.clone();
            term_changed |= analysis.terminator(&mut value, func, block, i, succ);
            if term_changed {
                if self.block_in[succ].meet_with(&value, analysis.meet_mode()) {
                    if !self.workqueue_set.contains(&succ) {
                        self.workqueue.push_back(succ);
                        self.workqueue_set.insert(succ);
                    }
                }
            }
        }
    }
}
