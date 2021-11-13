//! Metadata on operators.

use anyhow::{bail, Result};
use wasmparser::{FuncType, Operator, Type};

pub fn op_inputs(_sigs: &[FuncType], op: &Operator<'_>) -> Result<Vec<Type>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(vec![]),
        _ => bail!("Unknown operator in op_inputs(): {:?}", op),
    }
}

pub fn op_outputs(_sigs: &[FuncType], op: &Operator<'_>) -> Result<Vec<Type>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(vec![]),
        _ => bail!("Unknown operator in op_outputs(): {:?}", op),
    }
}
