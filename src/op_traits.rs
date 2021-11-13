//! Metadata on operators.

use crate::ir::{Module, SignatureId};
use anyhow::{bail, Result};
use wasmparser::{Operator, Type};

pub fn op_inputs(
    module: &Module,
    my_sig: SignatureId,
    my_locals: &[Type],
    op: &Operator<'_>,
) -> Result<Vec<Type>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(vec![]),

        &Operator::Call { function_index } => {
            let sig = module.funcs[function_index as usize].sig();
            Ok(Vec::from(module.signatures[sig].params.clone()))
        }
        &Operator::Return => Ok(Vec::from(module.signatures[my_sig].returns.clone())),

        &Operator::LocalSet { local_index } | &Operator::LocalTee { local_index } => {
            Ok(vec![my_locals[local_index as usize]])
        }
        &Operator::LocalGet { .. } => Ok(vec![]),

        &Operator::I32Eqz => Ok(vec![Type::I32]),
        &Operator::I32Eq => Ok(vec![Type::I32, Type::I32]),

        _ => bail!("Unknown operator in op_inputs(): {:?}", op),
    }
}

pub fn op_outputs(module: &Module, my_locals: &[Type], op: &Operator<'_>) -> Result<Vec<Type>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(vec![]),

        &Operator::Call { function_index } => {
            let sig = module.funcs[function_index as usize].sig();
            Ok(Vec::from(module.signatures[sig].returns.clone()))
        }
        &Operator::Return => Ok(vec![]),
        &Operator::LocalSet { .. } | &Operator::LocalTee { .. } => Ok(vec![]),
        &Operator::LocalGet { local_index } => Ok(vec![my_locals[local_index as usize]]),

        &Operator::I32Eqz | &Operator::I32Eq => Ok(vec![Type::I32]),

        _ => bail!("Unknown operator in op_outputs(): {:?}", op),
    }
}
