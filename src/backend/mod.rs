//! Backend: IR to Wasm.

use crate::cfg::CFGInfo;
use crate::ir::FunctionBody;
use crate::passes::rpo::RPO;
use anyhow::Result;

pub mod stackify;
use stackify::{Context as StackifyContext, WasmBlock};
pub mod treeify;
use treeify::Trees;
pub mod localify;
use localify::Localifier;

pub struct WasmBackend<'a> {
    body: &'a FunctionBody,
    rpo: RPO,
    trees: Trees,
    ctrl: Vec<WasmBlock<'a>>,
    locals: Localifier,
}

impl<'a> WasmBackend<'a> {
    pub fn new(body: &'a FunctionBody) -> Result<WasmBackend<'a>> {
        log::debug!("Backend compiling:\n{}\n", body.display_verbose("| "));
        let cfg = CFGInfo::new(body);
        let rpo = RPO::compute(body);
        log::debug!("RPO:\n{:?}\n", rpo);
        let trees = Trees::compute(body);
        log::debug!("Trees:\n{:?}\n", trees);
        let ctrl = StackifyContext::new(body, &cfg, &rpo)?.compute();
        log::debug!("Ctrl:\n{:?}\n", ctrl);
        let locals = Localifier::compute(body, &cfg, &trees);
        log::debug!("Locals:\n{:?}\n", locals);
        Ok(WasmBackend {
            body,
            rpo,
            trees,
            ctrl,
            locals,
        })
    }

    pub fn compile(&self) -> Result<Vec<u8>> {
        Ok(vec![])
    }
}
