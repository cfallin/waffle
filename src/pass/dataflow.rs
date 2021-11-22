//! Iterative dataflow analysis (forward and backward) using lattice
//! analysis values.

use crate::cfg::CFGInfo;
use crate::ir::*;
use crate::pass::Lattice;
use fxhash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry as HashEntry;
use std::marker::PhantomData;
use std::{collections::VecDeque, default::Default};
use wasmparser::Type;

impl FunctionBody {
    fn insts(&self) -> impl Iterator<Item = &Inst> {
        self.blocks.iter().map(|block| block.insts.iter()).flatten()
    }
}

pub trait DataflowFunctions {
    type L: Lattice;

    fn start_block(&self, _lattice: &mut Self::L, _block: BlockId, _param_types: &[Type]) {}
    fn end_block(
        &self,
        _lattce: &mut Self::L,
        _block: BlockId,
        _next: BlockId,
        _terminator: &Terminator,
    ) {
    }
    fn instruction(&self, _lattice: &mut Self::L, _block: BlockId, _instid: InstId, _inst: &Inst) {}
}

pub struct DataflowFunctionsImpl<L, F1, F2, F3> {
    f1: F1,
    f2: F2,
    f3: F3,
    _phantom: PhantomData<L>,
}

impl<L, F1, F2, F3> DataflowFunctionsImpl<L, F1, F2, F3> {
    pub fn new(f1: F1, f2: F2, f3: F3) -> Self {
        Self {
            f1,
            f2,
            f3,
            _phantom: PhantomData,
        }
    }
}

impl<L, F1, F2, F3> DataflowFunctions for DataflowFunctionsImpl<L, F1, F2, F3>
where
    L: Lattice,
    F1: Fn(&mut L, BlockId, InstId, &Inst),
    F2: Fn(&mut L, BlockId, &[Type]),
    F3: Fn(&mut L, BlockId, BlockId, &Terminator),
{
    type L = L;
    fn instruction(&self, lattice: &mut L, block: BlockId, instid: InstId, inst: &Inst) {
        (self.f1)(lattice, block, instid, inst);
    }
    fn start_block(&self, lattice: &mut L, block: BlockId, params: &[Type]) {
        (self.f2)(lattice, block, params);
    }
    fn end_block(&self, lattice: &mut L, block: BlockId, next: BlockId, terminator: &Terminator) {
        (self.f3)(lattice, block, next, terminator);
    }
}

#[macro_export]
macro_rules! dataflow {
    ($latticety:ty,
     |$lattice:ident, $block:ident, $instid:ident, $inst:ident| { $($body:tt)* }) => {
        DataflowFunctionsImpl::new(|$lattice:&mut $latticety, $block, $instid, $inst| {
            $($body)*
        }, |_, _, _| {}, |_, _, _, _| {})
    };

    ($latticety:ty,
     inst: |$lattice1:ident, $block1:ident, $instid1:ident, $inst1:ident| { $($body1:tt)* },
     start_block: |$lattice2:ident, $block2:ident, $params2:ident| { $($body2:tt)* }) => {
        DataflowFunctionsImpl::new(|$lattice1:&mut $latticety, $block1, $instid1, $inst1| {
            $($body1)*
        },
        |$lattice2, $block2, $params2| {
            $($body2)*
        }, |_, _, _, _| {})
    };

    ($latticety:ty,
     inst: |$lattice1:ident, $block1:ident, $instid1:ident, $inst1:ident| { $($body1:tt)* },
     start_block: |$lattice2:ident, $block2:ident, $params2:ident| { $($body2:tt)* },
     end_block: |$lattice3:ident, $block3:ident, $next3:ident, $term3:ident| { $($body3:tt)* }) => {
        DataflowFunctionsImpl::new(|$lattice1:&mut $latticety, $block1, $instid1, $inst1:&Inst| {
            $($body1)*
        },
        |$lattice2:&mut $latticety, $block2, $params2:&[wasmparser::Type]| {
            $($body2)*
        },
        |$lattice3:&mut $latticety, $block3, $next3, $term3:&Terminator| {
            $($body3)*
        })
    };
}

#[macro_export]
macro_rules! dataflow_use_def {
    ($lattice:ty,
     use: |$use:ident, $uselattice:ident| { $($usebody:tt)* },
     def: |$def:ident, $deflattice:ident| { $($defbody:tt)* }) => {
        {
            $crate::dataflow!(
                $lattice,
                inst: |lattice, block, instid, inst| {
                    let $deflattice = lattice;
                    for output in 0..inst.n_outputs {
                        let $def = $crate::ir::Value::inst(block, instid, output);
                        $($defbody)*
                    }
                    let $uselattice = $deflattice;
                    for &input in &inst.inputs {
                        let $use = input;
                        $($usebody)*
                    }
                },
                start_block: |lattice, block, param_tys| {
                    let $deflattice = lattice;
                    for i in 0..param_tys.len() {
                        let $def = $crate::ir::Value::blockparam(block, i);
                        $($defbody)*
                    }
                },
                end_block: |lattice, _block, _next, term| {
                    let $uselattice = lattice;
                    term.visit_uses(|u| {
                        let $use = u;
                        $($usebody)*
                    });
                }
            )
        }
    }
}

#[derive(Clone, Debug)]
pub struct ForwardDataflow<L: Lattice> {
    block_in: FxHashMap<BlockId, L>,
}

impl<L: Lattice> ForwardDataflow<L> {
    pub fn new<D: DataflowFunctions<L = L>>(f: &FunctionBody, d: &D) -> Self {
        let mut analysis = Self {
            block_in: FxHashMap::default(),
        };
        analysis.compute(f, d);
        analysis
    }

    fn compute<D: DataflowFunctions<L = L>>(&mut self, f: &FunctionBody, d: &D) {
        let mut workqueue = VecDeque::new();
        let mut workqueue_set = FxHashSet::default();

        workqueue.push_back(0);
        workqueue_set.insert(0);
        while let Some(block) = workqueue.pop_front() {
            workqueue_set.remove(&block);

            let mut value = self
                .block_in
                .entry(block)
                .or_insert_with(|| D::L::top())
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
                    std::mem::replace(&mut value, D::L::top())
                };

                d.end_block(&mut value, block, succ, &f.blocks[block].terminator);

                let (succ_in, mut changed) = match self.block_in.entry(succ) {
                    HashEntry::Vacant(v) => (v.insert(D::L::top()), true),
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
    pub fn new<D: DataflowFunctions<L = L>>(f: &FunctionBody, cfginfo: &CFGInfo, d: &D) -> Self {
        let mut analysis = Self {
            block_out: FxHashMap::default(),
        };
        analysis.compute(f, cfginfo, d);
        analysis
    }

    fn compute<D: DataflowFunctions<L = L>>(&mut self, f: &FunctionBody, cfginfo: &CFGInfo, d: &D) {
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
                .or_insert_with(|| D::L::top())
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
                    std::mem::replace(&mut value, D::L::top())
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

#[macro_export]
macro_rules! forward_pass {
    ($name:ident, $lattice:ident, $($dataflow:tt),*) => {
        #[derive(Clone, Debug)]
        pub struct $name($crate::pass::ForwardDataflow<$lattice>);

        impl $name {
            pub fn compute(f: &$crate::ir::FunctionBody) -> $name {
                let results = $crate::pass::ForwardDataflow::new(f, $($dataflow)*);
                Self(results)
            }
        }
    };
}

#[macro_export]
macro_rules! backward_pass {
    ($name:ident, $lattice:ident, $($dataflow:tt)*) => {
        #[derive(Clone, Debug)]
        pub struct $name($crate::pass::BackwardDataflow<$lattice>);

        impl $name {
            pub fn compute(f: &$crate::ir::FunctionBody, c: &$crate::cfg::CFGInfo) -> $name {
                let dataflow = $($dataflow)*;
                let results = $crate::pass::BackwardDataflow::new(f, c, &dataflow);
                Self(results)
            }
        }
    };
}
