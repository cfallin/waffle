//! Backend: IR to Wasm.

use crate::cfg::CFGInfo;
use crate::ir::{FunctionBody, Value};
use crate::passes::rpo::RPO;
use anyhow::Result;
use std::borrow::Cow;

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

    pub fn compile(&self) -> Result<wasm_encoder::Function> {
        let mut func = wasm_encoder::Function::new(
            self.locals
                .locals
                .values()
                .map(|&ty| (1, wasm_encoder::ValType::from(ty)))
                .collect::<Vec<_>>(),
        );

        for block in &self.ctrl {
            self.lower_block(block, &mut func);
        }

        // If the last block was a Block, Loop or If, then the type
        // may not match, so end with an Unreachable.
        match self.ctrl.last() {
            Some(&WasmBlock::Block { .. })
            | Some(&WasmBlock::Loop { .. })
            | Some(&WasmBlock::If { .. }) => {
                func.instruction(&wasm_encoder::Instruction::Unreachable);
            }
            _ => {}
        }

        Ok(func)
    }

    fn lower_block(&self, block: &WasmBlock<'_>, func: &mut wasm_encoder::Function) {
        match block {
            WasmBlock::Block { body, .. } => {
                func.instruction(&wasm_encoder::Instruction::Block(
                    wasm_encoder::BlockType::Empty,
                ));
                for sub_block in &body[..] {
                    self.lower_block(sub_block, func);
                }
                func.instruction(&wasm_encoder::Instruction::End);
            }
            WasmBlock::Loop { body, .. } => {
                func.instruction(&wasm_encoder::Instruction::Loop(
                    wasm_encoder::BlockType::Empty,
                ));
                for sub_block in &body[..] {
                    self.lower_block(sub_block, func);
                }
                func.instruction(&wasm_encoder::Instruction::End);
            }
            WasmBlock::Br { target } => {
                func.instruction(&wasm_encoder::Instruction::Br(target.index()));
            }
            WasmBlock::If {
                cond,
                if_true,
                if_false,
            } => {
                self.lower_value(*cond, func);
                func.instruction(&wasm_encoder::Instruction::If(
                    wasm_encoder::BlockType::Empty,
                ));
                for sub_block in &if_true[..] {
                    self.lower_block(sub_block, func);
                }
                if if_false.len() > 0 {
                    func.instruction(&wasm_encoder::Instruction::Else);
                    for sub_block in &if_false[..] {
                        self.lower_block(sub_block, func);
                    }
                }
                func.instruction(&wasm_encoder::Instruction::End);
            }
            WasmBlock::Select {
                selector,
                targets,
                default,
            } => {
                self.lower_value(*selector, func);
                func.instruction(&wasm_encoder::Instruction::BrTable(
                    Cow::Owned(
                        targets
                            .iter()
                            .map(|label| label.index())
                            .collect::<Vec<_>>(),
                    ),
                    default.index(),
                ));
            }
            WasmBlock::Leaf { block } => {
                for &inst in &self.body.blocks[*block].insts {
                    self.lower_inst(inst, func);
                }
            }
            WasmBlock::BlockParams { from, to } => {
                debug_assert_eq!(from.len(), to.len());
                for &from in from.iter() {
                    self.lower_value(from, func);
                }
                for &(_, to) in to.iter().rev() {
                    self.lower_set_value(to, func);
                }
            }
            WasmBlock::Return { values } => {
                for &value in &values[..] {
                    self.lower_value(value, func);
                }
                func.instruction(&wasm_encoder::Instruction::Return);
            }
            WasmBlock::Unreachable => {
                func.instruction(&wasm_encoder::Instruction::Unreachable);
            }
        }
    }

    fn lower_value(&self, value: Value, func: &mut wasm_encoder::Function) {
        todo!()
    }

    fn lower_set_value(&self, value: Value, func: &mut wasm_encoder::Function) {
        todo!()
    }

    fn lower_inst(&self, value: Value, func: &mut wasm_encoder::Function) {
        todo!()
    }
}
