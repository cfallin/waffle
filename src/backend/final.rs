//! Final Wasm operator sequence production.

use super::{Locations, SerializedBlockTarget, SerializedBody, SerializedOperator};
use crate::{ops::ty_to_valty, FunctionBody};
use std::borrow::Cow;
use wasm_encoder::BlockType;
use wasmparser::Type;

#[derive(Clone, Debug)]
pub struct Wasm {
    pub operators: Vec<wasm_encoder::Instruction<'static>>,
    pub locals: Vec<wasm_encoder::ValType>,
}

struct WasmContext<'a, FT: FuncTypeSink> {
    wasm: &'a mut Wasm,
    func_type_sink: &'a mut FT,
}

pub trait FuncTypeSink {
    fn add_signature(&mut self, params: Vec<Type>, results: Vec<Type>) -> u32;
}

impl<'a, FT: FuncTypeSink> WasmContext<'a, FT> {
    fn create_type(&mut self, params: Vec<Type>, results: Vec<Type>) -> u32 {
        self.func_type_sink.add_signature(params, results)
    }

    fn translate(&mut self, op: &SerializedOperator, locations: &Locations) {
        match op {
            SerializedOperator::StartBlock {
                ref params,
                ref results,
                ..
            } => {
                let ty =
                    self.create_type(params.iter().map(|&(ty, _)| ty).collect(), results.clone());
                self.wasm.operators.push(wasm_encoder::Instruction::Block(
                    BlockType::FunctionType(ty),
                ));
            }
            SerializedOperator::StartLoop {
                ref params,
                ref results,
                ..
            } => {
                let ty =
                    self.create_type(params.iter().map(|&(ty, _)| ty).collect(), results.clone());
                self.wasm
                    .operators
                    .push(wasm_encoder::Instruction::Loop(BlockType::FunctionType(ty)));
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
                self.wasm.operators.push(wasm_encoder::Instruction::If(
                    wasm_encoder::BlockType::Empty,
                ));
                self.translate_target(1, if_true, locations);
                self.wasm.operators.push(wasm_encoder::Instruction::Else);
                self.translate_target(1, if_false, locations);
                self.wasm.operators.push(wasm_encoder::Instruction::End);
            }
            SerializedOperator::BrTable {
                ref targets,
                ref default,
            } => {
                let ty = self.create_type(vec![Type::I32], vec![]);
                for _ in 0..(targets.len() + 2) {
                    self.wasm.operators.push(wasm_encoder::Instruction::Block(
                        wasm_encoder::BlockType::FunctionType(ty),
                    ));
                }

                let br_table_targets = (1..=targets.len()).map(|i| i as u32).collect::<Vec<_>>();
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
                self.wasm.operators.push(wasm_encoder::Instruction::Br(
                    (branch + extra_blocks) as u32,
                ));
            }
        }
    }
}

pub fn produce_func_wasm<FT: FuncTypeSink>(
    f: &FunctionBody,
    body: &SerializedBody,
    locations: &Locations,
    ft: &mut FT,
) -> Wasm {
    let mut wasm = Wasm {
        operators: vec![],
        locals: vec![],
    };
    wasm.locals
        .extend(f.locals.iter().map(|ty| ty_to_valty(*ty)));
    wasm.locals
        .extend(locations.new_locals.iter().map(|ty| ty_to_valty(*ty)));

    let mut ctx = WasmContext {
        wasm: &mut wasm,
        func_type_sink: ft,
    };
    for operator in &body.operators {
        ctx.translate(operator, locations);
    }
    
    // Fixup: add unreachable just before last `end`; there must be an explicit return.
//    assert!(matches!(wasm.operators.pop(), Some(wasm_encoder::Instruction::End)));
//    wasm.operators.push(wasm_encoder::Instruction::Unreachable);
//    wasm.operators.push(wasm_encoder::Instruction::End);

    wasm
}
