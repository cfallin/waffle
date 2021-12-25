//! Final Wasm operator sequence production.

use super::{Locations, SerializedBlockTarget, SerializedBody, SerializedOperator};
use crate::{ops::ty_to_valty, FunctionBody};
use std::borrow::Cow;
use wasm_encoder::BlockType;

#[derive(Clone, Debug)]
pub struct Wasm {
    pub operators: Vec<wasm_encoder::Instruction<'static>>,
    pub locals: Vec<wasm_encoder::ValType>,
}

struct WasmContext<'a> {
    wasm: &'a mut Wasm,
}

impl<'a> WasmContext<'a> {
    fn translate(&mut self, op: &SerializedOperator, locations: &Locations) {
        log::trace!("translate: {:?}", op);
        match op {
            SerializedOperator::StartBlock { .. } => {
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::Block(BlockType::Empty));
            }
            SerializedOperator::StartLoop { .. } => {
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::Loop(BlockType::Empty));
            }
            SerializedOperator::End => {
                self.wasm.operators.push(wasm_encoder::Instruction::End);
            }
            SerializedOperator::GetArg(index) => {
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::LocalGet(*index as u32));
            }
            SerializedOperator::Operator(op) => {
                self.wasm.operators.push(op.clone().into());
            }
            SerializedOperator::Br(ref target) => {
                self.translate_target(0, target, locations);
            }
            SerializedOperator::BrIf {
                ref if_true,
                ref if_false,
            } => {
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::If(BlockType::Empty));
                self.translate_target(1, if_true, locations);
                self.wasm.operators.push(wasm_encoder::Instruction::Else);
                self.translate_target(1, if_false, locations);
                self.wasm.operators.push(wasm_encoder::Instruction::End);
            }
            SerializedOperator::BrTable {
                ref index_ops,
                ref targets,
                ref default,
            } => {
                for _ in 0..(targets.len() + 2) {
                    self.wasm.operators.push(wasm_encoder::Instruction::Block(
                        wasm_encoder::BlockType::Empty,
                    ));
                }

                let br_table_targets = (1..=targets.len()).map(|i| i as u32).collect::<Vec<_>>();
                for op in index_ops {
                    self.translate(op, locations);
                }
                self.wasm.operators.push(wasm_encoder::Instruction::BrTable(
                    Cow::Owned(br_table_targets),
                    0,
                ));
                self.wasm.operators.push(wasm_encoder::Instruction::End);

                self.translate_target(targets.len() + 1, default, locations);
                self.wasm.operators.push(wasm_encoder::Instruction::End);

                for i in 0..targets.len() {
                    self.translate_target(targets.len() - i, &targets[i], locations);
                    self.wasm.operators.push(wasm_encoder::Instruction::End);
                }
            }
            SerializedOperator::Get(v, i) => {
                let loc = *locations.locations.get(&(*v, *i)).unwrap();
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::LocalGet(loc));
            }
            SerializedOperator::Set(v, i) => {
                let loc = *locations.locations.get(&(*v, *i)).unwrap();
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::LocalSet(loc));
            }
            SerializedOperator::Tee(v, i) => {
                let loc = *locations.locations.get(&(*v, *i)).unwrap();
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::LocalTee(loc));
            }
        }
    }

    fn translate_target(
        &mut self,
        extra_blocks: usize,
        target: &SerializedBlockTarget,
        locations: &Locations,
    ) {
        log::trace!("translate_target: {:?}", target);
        match target {
            &SerializedBlockTarget::Fallthrough(ref ops) => {
                for op in ops {
                    self.translate(op, locations);
                }
                if extra_blocks > 0 {
                    self.wasm
                        .operators
                        .push(wasm_encoder::Instruction::Br((extra_blocks - 1) as u32));
                }
            }
            &SerializedBlockTarget::Branch(branch, ref ops) => {
                for op in ops {
                    self.translate(op, locations);
                }
                self.wasm.operators.push(wasm_encoder::Instruction::Br(
                    (branch + extra_blocks) as u32,
                ));
            }
        }
    }
}

pub fn produce_func_wasm(f: &FunctionBody, body: &SerializedBody, locations: &Locations) -> Wasm {
    let mut wasm = Wasm {
        operators: vec![],
        locals: vec![],
    };
    wasm.locals
        .extend(f.locals.iter().skip(f.n_params).map(|ty| ty_to_valty(*ty)));
    wasm.locals
        .extend(locations.new_locals.iter().map(|ty| ty_to_valty(*ty)));

    let mut ctx = WasmContext { wasm: &mut wasm };
    for operator in &body.operators {
        ctx.translate(operator, locations);
    }
    // There is always an explicit Return before this point. This
    // allows us to avoid matching the return types in our stack
    // discipline / outer block type.
    wasm.operators.push(wasm_encoder::Instruction::Unreachable);
    wasm.operators.push(wasm_encoder::Instruction::End);

    wasm
}
