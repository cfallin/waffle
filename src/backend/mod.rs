//! Backend: IR to Wasm.

use crate::entity::{EntityRef, PerEntity};
use crate::ir::{Block, FunctionBody, Value, ValueDef};
use anyhow::Result;

pub mod stackify;
use stackify::{Mark, Marks, RPOIndex, RPO};
pub mod treeify;
use treeify::Trees;

pub struct WasmBackend<'a> {
    body: &'a FunctionBody,
    rpo: RPO,
    marks: Marks,
    trees: Trees,
}

impl<'a> WasmBackend<'a> {
    pub fn new(body: &'a FunctionBody) -> Result<WasmBackend<'a>> {
        let rpo = RPO::compute(body);
        let marks = Marks::compute(body, &rpo)?;
        let trees = Trees::compute(body);
        Ok(WasmBackend {
            body,
            rpo,
            marks,
            trees,
        })
    }

    pub fn compile(&self) -> Result<Vec<u8>> {
        Ok(vec![])
    }
}
