//! Local-to-SSA conversion.

use crate::{
    dataflow::{AnalysisFunction, AnalysisValue, ForwardDataflow, Lattice},
    FunctionBody, Operand,
};
use crate::{BlockId, InstId, ValueId};
use wasmparser::Operator;

// We do a really simple thing for now:
// - Compute "is-there-more-than-one-reaching-definition" property for
//   every var at each use site. We do this by tracking a lattice: no
//   defs known (top), exactly one def known, multiple defs known
//   (bottom).
// - For every var for which there is more than one
//   reaching-definition at any use, insert a blockparam on every
//   block where the var is live-in.
// - Annotate SSA values as belonging to a disjoint-set (only one live
//   at a time) to assist lowering back into Wasm (so they can share a
//   local).

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReachingDefsLattice {
    Unknown,
    OneDef(ValueId),
    ManyDefs,
}

impl std::default::Default for ReachingDefsLattice {
    fn default() -> Self {
        Self::Unknown
    }
}

impl Lattice for ReachingDefsLattice {
    fn top() -> Self {
        Self::Unknown
    }

    fn bottom() -> Self {
        Self::ManyDefs
    }

    fn meet(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (a, Self::Unknown) => *a,
            (Self::Unknown, b) => *b,

            (Self::OneDef(a), Self::OneDef(b)) if a == b => Self::OneDef(*a),
            (Self::OneDef(_), Self::OneDef(_)) => Self::ManyDefs,

            (Self::ManyDefs, _) | (_, Self::ManyDefs) => Self::ManyDefs,
        }
    }
}

struct LocalReachingDefsAnalysis;
impl AnalysisFunction for LocalReachingDefsAnalysis {
    type K = u32; // Local index
    type L = ReachingDefsLattice;

    fn instruction(
        &self,
        input: &mut AnalysisValue<Self::K, Self::L>,
        func: &FunctionBody,
        block: BlockId,
        inst: InstId,
    ) -> bool {
        let inst = &func.blocks[block].insts[inst];
        match &inst.operator {
            &Operator::LocalSet { local_index } | &Operator::LocalTee { local_index } => {
                if let Operand::Value(value) = inst.inputs[0] {
                    let value = ReachingDefsLattice::OneDef(value);
                    let old = input.values.insert(local_index, value);
                    Some(value) != old
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

pub struct LocalSSATransform {
    local_analysis: ForwardDataflow<LocalReachingDefsAnalysis>,
}

impl LocalSSATransform {
    pub fn new(func: &FunctionBody) -> Self {
        LocalSSATransform {
            local_analysis: ForwardDataflow::new(&LocalReachingDefsAnalysis, func),
        }
    }
}
