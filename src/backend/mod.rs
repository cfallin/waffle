//! Backend: IR to Wasm.

use crate::entity::{EntityRef, PerEntity};
use crate::ir::{Block, FunctionBody, Value, ValueDef};
use crate::passes::rpo::{RPOIndex, RPO};
use anyhow::Result;

pub mod stackify;
pub mod treeify;
use treeify::Trees;

pub struct WasmBackend<'a> {
    body: &'a FunctionBody,
    rpo: RPO,
    trees: Trees,
}

impl<'a> WasmBackend<'a> {
    pub fn new(body: &'a FunctionBody) -> Result<WasmBackend<'a>> {
        let rpo = RPO::compute(body);
        let trees = Trees::compute(body);
        Ok(WasmBackend { body, rpo, trees })
    }

    pub fn compile(&self) -> Result<Vec<u8>> {
        Ok(vec![])
    }
}
