//! Final Wasm operator sequence production.

use super::{Locations, SerializedBlockTarget, SerializedBody, SerializedOperator};
use crate::{ops::ty_to_valty, FunctionBody};
use std::borrow::Cow;
use wasm_encoder::BlockType;

#[derive(Clone, Debug)]
pub struct Wasm {
    operators: Vec<wasm_encoder::Instruction<'static>>,
    locals: Vec<wasm_encoder::ValType>,
    func_types: Vec<(Vec<wasm_encoder::ValType>, Vec<wasm_encoder::ValType>)>,
}

impl Wasm {
    fn create_type(
        &mut self,
        params: Vec<wasm_encoder::ValType>,
        results: Vec<wasm_encoder::ValType>,
    ) -> u32 {
        let idx = self.func_types.len() as u32;
        self.func_types.push((params, results));
        idx
    }
    fn translate(&mut self, op: &SerializedOperator, locations: &Locations) {
        match op {
            SerializedOperator::StartBlock {
                ref params,
                ref results,
                ..
            } => {
                let ty = self.create_type(
                    params.iter().map(|(ty, _)| ty_to_valty(*ty)).collect(),
                    results.iter().map(|ty| ty_to_valty(*ty)).collect(),
                );
                self.operators
                    .push(wasm_encoder::Instruction::Block(BlockType::FunctionType(
                        ty,
                    )));
            }
            SerializedOperator::StartLoop {
                ref params,
                ref results,
                ..
            } => {
                let ty = self.create_type(
                    params.iter().map(|(ty, _)| ty_to_valty(*ty)).collect(),
                    results.iter().map(|ty| ty_to_valty(*ty)).collect(),
                );
                self.operators
                    .push(wasm_encoder::Instruction::Loop(BlockType::FunctionType(ty)));
            }
            SerializedOperator::End => {
                self.operators.push(wasm_encoder::Instruction::End);
            }
            SerializedOperator::GetArg(index) => {
                self.operators
                    .push(wasm_encoder::Instruction::LocalGet(*index as u32));
            }
            SerializedOperator::Operator(op) => {
                self.operators.push(op.clone().into());
            }
            SerializedOperator::Br(ref target) => {
                self.translate_target(0, target, locations);
            }
            SerializedOperator::BrIf {
                ref if_true,
                ref if_false,
            } => {
                self.operators.push(wasm_encoder::Instruction::If(
                    wasm_encoder::BlockType::Empty,
                ));
                self.translate_target(1, if_true, locations);
                self.operators.push(wasm_encoder::Instruction::Else);
                self.translate_target(1, if_false, locations);
                self.operators.push(wasm_encoder::Instruction::End);
            }
            SerializedOperator::BrTable {
                ref targets,
                ref default,
            } => {
                let ty = self.create_type(vec![wasm_encoder::ValType::I32], vec![]);
                for _ in 0..(targets.len() + 2) {
                    self.operators.push(wasm_encoder::Instruction::Block(
                        wasm_encoder::BlockType::FunctionType(ty),
                    ));
                }

                let br_table_targets = (1..=targets.len()).map(|i| i as u32).collect::<Vec<_>>();
                self.operators.push(wasm_encoder::Instruction::BrTable(
                    Cow::Owned(br_table_targets),
                    0,
                ));
                self.operators.push(wasm_encoder::Instruction::End);

                self.translate_target(targets.len() + 1, default, locations);
                self.operators.push(wasm_encoder::Instruction::End);

                for i in 0..targets.len() {
                    self.translate_target(targets.len() - i, &targets[i], locations);
                    self.operators.push(wasm_encoder::Instruction::End);
                }
            }
            SerializedOperator::Get(v, i) => {
                let loc = *locations.locations.get(&(*v, *i)).unwrap();
                self.operators
                    .push(wasm_encoder::Instruction::LocalGet(loc));
            }
            SerializedOperator::Set(v, i) => {
                let loc = *locations.locations.get(&(*v, *i)).unwrap();
                self.operators
                    .push(wasm_encoder::Instruction::LocalSet(loc));
            }
            SerializedOperator::Tee(v, i) => {
                let loc = *locations.locations.get(&(*v, *i)).unwrap();
                self.operators
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
        match target {
            &SerializedBlockTarget::Fallthrough(ref ops) => {
                for op in ops {
                    self.translate(op, locations);
                }
            }
            &SerializedBlockTarget::Branch(branch, ref ops) => {
                for op in ops {
                    self.translate(op, locations);
                }
                self.operators.push(wasm_encoder::Instruction::Br(
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
        func_types: vec![],
    };

    wasm.locals
        .extend(f.locals.iter().map(|ty| ty_to_valty(*ty)));
    wasm.locals
        .extend(locations.new_locals.iter().map(|ty| ty_to_valty(*ty)));

    for operator in &body.operators {
        wasm.translate(operator, locations);
    }

    wasm
}
