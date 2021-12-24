//! Final Wasm operator sequence production.

use wasm_encoder::BlockType;

use crate::{ops::ty_to_valty, FunctionBody, Operator};

use super::{Locations, SerializedBody, SerializedOperator};

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
                todo!()
            }
            SerializedOperator::BrIf {
                cond,
                ref if_true,
                ref if_false,
            } => {
                todo!()
            }
            SerializedOperator::BrTable {
                index,
                ref targets,
                ref default,
            } => {
                todo!()
            }
            SerializedOperator::Get(v, i) => {
                todo!()
            }
            SerializedOperator::Set(v, i) => {
                todo!()
            }
            SerializedOperator::Tee(v, i) => {
                todo!()
            }
        }
    }
}

pub fn produce_wasm(f: &FunctionBody, body: &SerializedBody, locations: &Locations) -> Wasm {
    let mut wasm = Wasm {
        operators: vec![],
        locals: vec![],
        func_types: vec![],
    };

    wasm.locals
        .extend(f.locals.iter().map(|ty| ty_to_valty(*ty)));
    wasm.locals
        .extend(locations.new_locals.iter().map(|ty| ty_to_valty(*ty)));

    let mut next_delete = 0;
    for (index, operator) in body.operators.iter().enumerate() {
        if next_delete < locations.delete.len() && locations.delete[next_delete] == index {
            next_delete += 1;
            continue;
        }
        wasm.translate(operator, locations);
    }

    wasm
}
