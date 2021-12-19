//! Serialization of the sea-of-nodes IR using a BlockOrder
//! Wasm-structured-control-flow result into actual order of operators
//! in Wasm function body. Contains everything needed to emit Wasm
//! except for value locations (and corresponding local spill/reloads).

use super::{
    structured::{BlockOrder, BlockOrderEntry, BlockOrderTarget},
    CFGInfo,
};
use crate::{BlockId, FunctionBody, Value};

#[derive(Clone, Debug)]
pub struct SerializedBody {
    pub(crate) operators: Vec<SerializedOperator>,
}

#[derive(Clone, Debug)]
pub enum SerializedBlockTarget {
    Fallthrough(Vec<Value>),
    Branch(usize, Vec<Value>),
}

#[derive(Clone, Debug)]
pub enum SerializedOperator {
    StartBlock {
        header: BlockId,
        params: Vec<(wasmparser::Type, Value)>,
    },
    StartLoop {
        header: BlockId,
        param: Vec<(wasmparser::Type, Value)>,
    },
    Br(SerializedBlockTarget),
    BrIf {
        cond: Value,
        if_true: SerializedBlockTarget,
        if_false: SerializedBlockTarget,
    },
    BrTable {
        index: Value,
        targets: Vec<SerializedBlockTarget>,
        default: SerializedBlockTarget,
    },
    Operator(Value),
    End,
}

impl SerializedBody {
    pub fn compute(f: &FunctionBody, cfg: &CFGInfo, order: &BlockOrder) -> SerializedBody {
        let mut operators = vec![];
        for entry in &order.entries {
            Self::compute_entry(f, cfg, entry, &mut operators);
        }
        SerializedBody { operators }
    }

    fn compute_entry(
        f: &FunctionBody,
        cfg: &CFGInfo,
        entry: &BlockOrderEntry,
        operators: &mut Vec<SerializedOperator>,
    ) {
        match entry {
            &BlockOrderEntry::StartBlock(header, ref params) => {
                operators.push(SerializedOperator::StartBlock {
                    header,
                    params: params.clone(),
                });
            }
            &BlockOrderEntry::StartLoop(header, ref params) => {
                operators.push(SerializedOperator::StartBlock {
                    header,
                    params: params.clone(),
                });
            }
            &BlockOrderEntry::End => {
                operators.push(SerializedOperator::End);
            }
            &BlockOrderEntry::BasicBlock(block, ref targets) => {
                todo!()
            }
        }
    }
}
