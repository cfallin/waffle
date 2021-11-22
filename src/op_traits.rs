//! Metadata on operators.

use crate::ir::{Module, SignatureId, Value};
use anyhow::{bail, Result};
use wasmparser::{Operator, Type};

pub fn op_inputs(
    module: &Module,
    my_sig: SignatureId,
    my_locals: &[Type],
    op_stack: &[(Type, Value)],
    op: &Operator<'_>,
) -> Result<Vec<Type>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(vec![]),

        &Operator::Call { function_index } => {
            let sig = module.funcs[function_index as usize].sig();
            Ok(Vec::from(module.signatures[sig].params.clone()))
        }
        &Operator::CallIndirect { index, .. } => {
            let mut params = module.signatures[index as usize].params.to_vec();
            params.push(Type::I32);
            Ok(params)
        }
        &Operator::Return => Ok(Vec::from(module.signatures[my_sig].returns.clone())),

        &Operator::LocalSet { local_index } | &Operator::LocalTee { local_index } => {
            Ok(vec![my_locals[local_index as usize]])
        }
        &Operator::LocalGet { .. } => Ok(vec![]),

        &Operator::Select => {
            let val_ty = op_stack[op_stack.len() - 2].0;
            Ok(vec![val_ty, val_ty, Type::I32])
        }
        &Operator::TypedSelect { ty } => Ok(vec![ty, ty, Type::I32]),

        &Operator::GlobalGet { .. } => Ok(vec![]),
        &Operator::GlobalSet { global_index } => Ok(vec![module.globals[global_index as usize]]),

        Operator::I32Load { .. }
        | Operator::I64Load { .. }
        | Operator::F32Load { .. }
        | Operator::F64Load { .. }
        | Operator::I32Load8S { .. }
        | Operator::I32Load8U { .. }
        | Operator::I32Load16S { .. }
        | Operator::I32Load16U { .. }
        | Operator::I64Load8S { .. }
        | Operator::I64Load8U { .. }
        | Operator::I64Load16S { .. }
        | Operator::I64Load16U { .. }
        | Operator::I64Load32S { .. }
        | Operator::I64Load32U { .. } => Ok(vec![Type::I32]),

        Operator::I32Store { .. } => Ok(vec![Type::I32, Type::I32]),
        Operator::I64Store { .. } => Ok(vec![Type::I32, Type::I64]),
        Operator::F32Store { .. } => Ok(vec![Type::I32, Type::F32]),
        Operator::F64Store { .. } => Ok(vec![Type::I32, Type::F64]),
        Operator::I32Store8 { .. } => Ok(vec![Type::I32, Type::I32]),
        Operator::I32Store16 { .. } => Ok(vec![Type::I32, Type::I32]),
        Operator::I64Store8 { .. } => Ok(vec![Type::I32, Type::I64]),
        Operator::I64Store16 { .. } => Ok(vec![Type::I32, Type::I64]),
        Operator::I64Store32 { .. } => Ok(vec![Type::I32, Type::I64]),

        Operator::I32Const { .. }
        | Operator::I64Const { .. }
        | Operator::F32Const { .. }
        | Operator::F64Const { .. } => Ok(vec![]),

        Operator::I32Eqz => Ok(vec![Type::I32]),
        Operator::I32Eq
        | Operator::I32Ne
        | Operator::I32LtS
        | Operator::I32LtU
        | Operator::I32GtS
        | Operator::I32GtU
        | Operator::I32LeS
        | Operator::I32LeU
        | Operator::I32GeS
        | Operator::I32GeU => Ok(vec![Type::I32, Type::I32]),

        Operator::I64Eqz => Ok(vec![Type::I64]),

        Operator::I64Eq
        | Operator::I64Ne
        | Operator::I64LtS
        | Operator::I64LtU
        | Operator::I64GtU
        | Operator::I64GtS
        | Operator::I64LeS
        | Operator::I64LeU
        | Operator::I64GeS
        | Operator::I64GeU => Ok(vec![Type::I64, Type::I64]),

        Operator::F32Eq
        | Operator::F32Ne
        | Operator::F32Lt
        | Operator::F32Gt
        | Operator::F32Le
        | Operator::F32Ge => Ok(vec![Type::F32, Type::F32]),

        Operator::F64Eq
        | Operator::F64Ne
        | Operator::F64Lt
        | Operator::F64Gt
        | Operator::F64Le
        | Operator::F64Ge => Ok(vec![Type::F64, Type::F64]),

        Operator::I32Clz | Operator::I32Ctz | Operator::I32Popcnt => Ok(vec![Type::I32]),

        Operator::I32Add
        | Operator::I32Sub
        | Operator::I32Mul
        | Operator::I32DivS
        | Operator::I32DivU
        | Operator::I32RemS
        | Operator::I32RemU
        | Operator::I32And
        | Operator::I32Or
        | Operator::I32Xor
        | Operator::I32Shl
        | Operator::I32ShrS
        | Operator::I32ShrU
        | Operator::I32Rotl
        | Operator::I32Rotr => Ok(vec![Type::I32, Type::I32]),

        Operator::I64Clz | Operator::I64Ctz | Operator::I64Popcnt => Ok(vec![Type::I64]),

        Operator::I64Add
        | Operator::I64Sub
        | Operator::I64Mul
        | Operator::I64DivS
        | Operator::I64DivU
        | Operator::I64RemS
        | Operator::I64RemU
        | Operator::I64And
        | Operator::I64Or
        | Operator::I64Xor
        | Operator::I64Shl
        | Operator::I64ShrS
        | Operator::I64ShrU
        | Operator::I64Rotl
        | Operator::I64Rotr => Ok(vec![Type::I64, Type::I64]),

        Operator::F32Abs
        | Operator::F32Neg
        | Operator::F32Ceil
        | Operator::F32Floor
        | Operator::F32Trunc
        | Operator::F32Nearest
        | Operator::F32Sqrt => Ok(vec![Type::F32]),

        Operator::F32Add
        | Operator::F32Sub
        | Operator::F32Mul
        | Operator::F32Div
        | Operator::F32Min
        | Operator::F32Max
        | Operator::F32Copysign => Ok(vec![Type::F32, Type::F32]),

        Operator::F64Abs
        | Operator::F64Neg
        | Operator::F64Ceil
        | Operator::F64Floor
        | Operator::F64Trunc
        | Operator::F64Nearest
        | Operator::F64Sqrt => Ok(vec![Type::F64]),

        Operator::F64Add
        | Operator::F64Sub
        | Operator::F64Mul
        | Operator::F64Div
        | Operator::F64Min
        | Operator::F64Max
        | Operator::F64Copysign => Ok(vec![Type::F64, Type::F64]),

        Operator::I32WrapI64 => Ok(vec![Type::I64]),
        Operator::I32TruncF32S => Ok(vec![Type::F32]),
        Operator::I32TruncF32U => Ok(vec![Type::F32]),
        Operator::I32TruncF64S => Ok(vec![Type::F64]),
        Operator::I32TruncF64U => Ok(vec![Type::F64]),
        Operator::I64ExtendI32S => Ok(vec![Type::I32]),
        Operator::I64ExtendI32U => Ok(vec![Type::I32]),
        Operator::I64TruncF32S => Ok(vec![Type::F32]),
        Operator::I64TruncF32U => Ok(vec![Type::F32]),
        Operator::I64TruncF64S => Ok(vec![Type::F64]),
        Operator::I64TruncF64U => Ok(vec![Type::F64]),
        Operator::F32ConvertI32S => Ok(vec![Type::I32]),
        Operator::F32ConvertI32U => Ok(vec![Type::I32]),
        Operator::F32ConvertI64S => Ok(vec![Type::I64]),
        Operator::F32ConvertI64U => Ok(vec![Type::I64]),
        Operator::F32DemoteF64 => Ok(vec![Type::F64]),
        Operator::F64ConvertI32S => Ok(vec![Type::I32]),
        Operator::F64ConvertI32U => Ok(vec![Type::I32]),
        Operator::F64ConvertI64S => Ok(vec![Type::I64]),
        Operator::F64ConvertI64U => Ok(vec![Type::I64]),
        Operator::F64PromoteF32 => Ok(vec![Type::F32]),
        Operator::I32Extend8S => Ok(vec![Type::I32]),
        Operator::I32Extend16S => Ok(vec![Type::I32]),
        Operator::I64Extend8S => Ok(vec![Type::I64]),
        Operator::I64Extend16S => Ok(vec![Type::I64]),
        Operator::I64Extend32S => Ok(vec![Type::I64]),
        Operator::I32TruncSatF32S => Ok(vec![Type::F32]),
        Operator::I32TruncSatF32U => Ok(vec![Type::F32]),
        Operator::I32TruncSatF64S => Ok(vec![Type::F64]),
        Operator::I32TruncSatF64U => Ok(vec![Type::F64]),
        Operator::I64TruncSatF32S => Ok(vec![Type::F32]),
        Operator::I64TruncSatF32U => Ok(vec![Type::F32]),
        Operator::I64TruncSatF64S => Ok(vec![Type::F64]),
        Operator::I64TruncSatF64U => Ok(vec![Type::F64]),
        Operator::F32ReinterpretI32 => Ok(vec![Type::I32]),
        Operator::F64ReinterpretI64 => Ok(vec![Type::I64]),
        Operator::I32ReinterpretF32 => Ok(vec![Type::F32]),
        Operator::I64ReinterpretF64 => Ok(vec![Type::F64]),
        Operator::TableGet { .. } => Ok(vec![Type::I32]),
        Operator::TableSet { table } => Ok(vec![Type::I32, module.tables[*table as usize]]),
        Operator::TableGrow { .. } => Ok(vec![Type::I32]),
        Operator::TableSize { .. } => Ok(vec![]),
        Operator::MemorySize { .. } => Ok(vec![]),
        Operator::MemoryGrow { .. } => Ok(vec![Type::I32]),

        _ => bail!("Unknown operator in op_inputs(): {:?}", op),
    }
}

pub fn op_outputs(
    module: &Module,
    my_locals: &[Type],
    op_stack: &[(Type, Value)],
    op: &Operator<'_>,
) -> Result<Vec<Type>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(vec![]),

        &Operator::Call { function_index } => {
            let sig = module.funcs[function_index as usize].sig();
            Ok(Vec::from(module.signatures[sig].returns.clone()))
        }
        &Operator::CallIndirect { index, .. } => {
            Ok(Vec::from(module.signatures[index as usize].returns.clone()))
        }
        &Operator::Return => Ok(vec![]),
        &Operator::LocalSet { .. } => Ok(vec![]),
        &Operator::LocalGet { local_index } | &Operator::LocalTee { local_index } => {
            Ok(vec![my_locals[local_index as usize]])
        }

        &Operator::Select => {
            let val_ty = op_stack[op_stack.len() - 2].0;
            Ok(vec![val_ty])
        }
        &Operator::TypedSelect { ty } => Ok(vec![ty]),
        &Operator::GlobalGet { global_index } => Ok(vec![module.globals[global_index as usize]]),
        &Operator::GlobalSet { .. } => Ok(vec![]),

        Operator::I32Load { .. }
        | Operator::I32Load8S { .. }
        | Operator::I32Load8U { .. }
        | Operator::I32Load16S { .. }
        | Operator::I32Load16U { .. } => Ok(vec![Type::I32]),
        Operator::I64Load { .. }
        | Operator::I64Load8S { .. }
        | Operator::I64Load8U { .. }
        | Operator::I64Load16S { .. }
        | Operator::I64Load16U { .. }
        | Operator::I64Load32S { .. }
        | Operator::I64Load32U { .. } => Ok(vec![Type::I64]),
        Operator::F32Load { .. } => Ok(vec![Type::F32]),
        Operator::F64Load { .. } => Ok(vec![Type::F64]),

        Operator::I32Store { .. } => Ok(vec![]),
        Operator::I64Store { .. } => Ok(vec![]),
        Operator::F32Store { .. } => Ok(vec![]),
        Operator::F64Store { .. } => Ok(vec![]),
        Operator::I32Store8 { .. } => Ok(vec![]),
        Operator::I32Store16 { .. } => Ok(vec![]),
        Operator::I64Store8 { .. } => Ok(vec![]),
        Operator::I64Store16 { .. } => Ok(vec![]),
        Operator::I64Store32 { .. } => Ok(vec![]),

        Operator::I32Const { .. } => Ok(vec![Type::I32]),
        Operator::I64Const { .. } => Ok(vec![Type::I64]),
        Operator::F32Const { .. } => Ok(vec![Type::F32]),
        Operator::F64Const { .. } => Ok(vec![Type::F64]),

        Operator::I32Eqz
        | Operator::I32Eq
        | Operator::I32Ne
        | Operator::I32LtS
        | Operator::I32LtU
        | Operator::I32GtS
        | Operator::I32GtU
        | Operator::I32LeS
        | Operator::I32LeU
        | Operator::I32GeS
        | Operator::I32GeU
        | Operator::I64Eqz
        | Operator::I64Eq
        | Operator::I64Ne
        | Operator::I64LtS
        | Operator::I64LtU
        | Operator::I64GtU
        | Operator::I64GtS
        | Operator::I64LeS
        | Operator::I64LeU
        | Operator::I64GeS
        | Operator::I64GeU
        | Operator::F32Eq
        | Operator::F32Ne
        | Operator::F32Lt
        | Operator::F32Gt
        | Operator::F32Le
        | Operator::F32Ge
        | Operator::F64Eq
        | Operator::F64Ne
        | Operator::F64Lt
        | Operator::F64Gt
        | Operator::F64Le
        | Operator::F64Ge => Ok(vec![Type::I32]),

        Operator::I32Clz
        | Operator::I32Ctz
        | Operator::I32Popcnt
        | Operator::I32Add
        | Operator::I32Sub
        | Operator::I32Mul
        | Operator::I32DivS
        | Operator::I32DivU
        | Operator::I32RemS
        | Operator::I32RemU
        | Operator::I32And
        | Operator::I32Or
        | Operator::I32Xor
        | Operator::I32Shl
        | Operator::I32ShrS
        | Operator::I32ShrU
        | Operator::I32Rotl
        | Operator::I32Rotr => Ok(vec![Type::I32]),

        Operator::I64Clz
        | Operator::I64Ctz
        | Operator::I64Popcnt
        | Operator::I64Add
        | Operator::I64Sub
        | Operator::I64Mul
        | Operator::I64DivS
        | Operator::I64DivU
        | Operator::I64RemS
        | Operator::I64RemU
        | Operator::I64And
        | Operator::I64Or
        | Operator::I64Xor
        | Operator::I64Shl
        | Operator::I64ShrS
        | Operator::I64ShrU
        | Operator::I64Rotl
        | Operator::I64Rotr => Ok(vec![Type::I64]),

        Operator::F32Abs
        | Operator::F32Neg
        | Operator::F32Ceil
        | Operator::F32Floor
        | Operator::F32Trunc
        | Operator::F32Nearest
        | Operator::F32Sqrt
        | Operator::F32Add
        | Operator::F32Sub
        | Operator::F32Mul
        | Operator::F32Div
        | Operator::F32Min
        | Operator::F32Max
        | Operator::F32Copysign => Ok(vec![Type::F32]),

        Operator::F64Abs
        | Operator::F64Neg
        | Operator::F64Ceil
        | Operator::F64Floor
        | Operator::F64Trunc
        | Operator::F64Nearest
        | Operator::F64Sqrt
        | Operator::F64Add
        | Operator::F64Sub
        | Operator::F64Mul
        | Operator::F64Div
        | Operator::F64Min
        | Operator::F64Max
        | Operator::F64Copysign => Ok(vec![Type::F64]),

        Operator::I32WrapI64 => Ok(vec![Type::I32]),
        Operator::I32TruncF32S => Ok(vec![Type::I32]),
        Operator::I32TruncF32U => Ok(vec![Type::I32]),
        Operator::I32TruncF64S => Ok(vec![Type::I32]),
        Operator::I32TruncF64U => Ok(vec![Type::I32]),
        Operator::I64ExtendI32S => Ok(vec![Type::I64]),
        Operator::I64ExtendI32U => Ok(vec![Type::I64]),
        Operator::I64TruncF32S => Ok(vec![Type::I64]),
        Operator::I64TruncF32U => Ok(vec![Type::I64]),
        Operator::I64TruncF64S => Ok(vec![Type::I64]),
        Operator::I64TruncF64U => Ok(vec![Type::I64]),
        Operator::F32ConvertI32S => Ok(vec![Type::F32]),
        Operator::F32ConvertI32U => Ok(vec![Type::F32]),
        Operator::F32ConvertI64S => Ok(vec![Type::F32]),
        Operator::F32ConvertI64U => Ok(vec![Type::F32]),
        Operator::F32DemoteF64 => Ok(vec![Type::F32]),
        Operator::F64ConvertI32S => Ok(vec![Type::F64]),
        Operator::F64ConvertI32U => Ok(vec![Type::F64]),
        Operator::F64ConvertI64S => Ok(vec![Type::F64]),
        Operator::F64ConvertI64U => Ok(vec![Type::F64]),
        Operator::F64PromoteF32 => Ok(vec![Type::F64]),
        Operator::I32Extend8S => Ok(vec![Type::I32]),
        Operator::I32Extend16S => Ok(vec![Type::I32]),
        Operator::I64Extend8S => Ok(vec![Type::I64]),
        Operator::I64Extend16S => Ok(vec![Type::I64]),
        Operator::I64Extend32S => Ok(vec![Type::I64]),
        Operator::I32TruncSatF32S => Ok(vec![Type::I32]),
        Operator::I32TruncSatF32U => Ok(vec![Type::I32]),
        Operator::I32TruncSatF64S => Ok(vec![Type::I32]),
        Operator::I32TruncSatF64U => Ok(vec![Type::I32]),
        Operator::I64TruncSatF32S => Ok(vec![Type::I64]),
        Operator::I64TruncSatF32U => Ok(vec![Type::I64]),
        Operator::I64TruncSatF64S => Ok(vec![Type::I64]),
        Operator::I64TruncSatF64U => Ok(vec![Type::I64]),
        Operator::F32ReinterpretI32 => Ok(vec![Type::F32]),
        Operator::F64ReinterpretI64 => Ok(vec![Type::F64]),
        Operator::I32ReinterpretF32 => Ok(vec![Type::I32]),
        Operator::I64ReinterpretF64 => Ok(vec![Type::I64]),
        Operator::TableGet { table } => Ok(vec![module.tables[*table as usize]]),
        Operator::TableSet { .. } => Ok(vec![]),
        Operator::TableGrow { .. } => Ok(vec![]),
        Operator::TableSize { .. } => Ok(vec![Type::I32]),
        Operator::MemorySize { .. } => Ok(vec![Type::I32]),
        Operator::MemoryGrow { .. } => Ok(vec![Type::I32]),

        _ => bail!("Unknown operator in op_outputs(): {:?}", op),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SideEffect {
    Trap,
    ReadMem,
    WriteMem,
    ReadGlobal(usize),
    WriteGlobal(usize),
    ReadTable(usize),
    WriteTable(usize),
    ReadLocal(usize),
    WriteLocal(usize),
    Return,
    All,
}

pub fn op_effects(op: &Operator<'_>) -> Result<Vec<SideEffect>> {
    use SideEffect::*;

    match op {
        &Operator::Unreachable => Ok(vec![Trap]),
        &Operator::Nop => Ok(vec![]),

        &Operator::Call { .. } => Ok(vec![All]),
        &Operator::CallIndirect { .. } => Ok(vec![All]),
        &Operator::Return => Ok(vec![Return]),
        &Operator::LocalSet { local_index, .. } => Ok(vec![WriteLocal(local_index as usize)]),
        &Operator::LocalGet { local_index, .. } => Ok(vec![ReadLocal(local_index as usize)]),
        &Operator::LocalTee { local_index, .. } => Ok(vec![
            ReadLocal(local_index as usize),
            WriteLocal(local_index as usize),
        ]),

        &Operator::Select => Ok(vec![]),
        &Operator::TypedSelect { .. } => Ok(vec![]),
        &Operator::GlobalGet { global_index, .. } => Ok(vec![ReadGlobal(global_index as usize)]),
        &Operator::GlobalSet { global_index, .. } => Ok(vec![WriteGlobal(global_index as usize)]),

        Operator::I32Load { .. }
        | Operator::I32Load8S { .. }
        | Operator::I32Load8U { .. }
        | Operator::I32Load16S { .. }
        | Operator::I32Load16U { .. }
        | Operator::I64Load { .. }
        | Operator::I64Load8S { .. }
        | Operator::I64Load8U { .. }
        | Operator::I64Load16S { .. }
        | Operator::I64Load16U { .. }
        | Operator::I64Load32S { .. }
        | Operator::I64Load32U { .. }
        | Operator::F32Load { .. }
        | Operator::F64Load { .. } => Ok(vec![Trap, ReadMem]),

        Operator::I32Store { .. }
        | Operator::I64Store { .. }
        | Operator::F32Store { .. }
        | Operator::F64Store { .. }
        | Operator::I32Store8 { .. }
        | Operator::I32Store16 { .. }
        | Operator::I64Store8 { .. }
        | Operator::I64Store16 { .. }
        | Operator::I64Store32 { .. } => Ok(vec![Trap, WriteMem]),

        Operator::I32Const { .. }
        | Operator::I64Const { .. }
        | Operator::F32Const { .. }
        | Operator::F64Const { .. } => Ok(vec![]),

        Operator::I32Eqz
        | Operator::I32Eq
        | Operator::I32Ne
        | Operator::I32LtS
        | Operator::I32LtU
        | Operator::I32GtS
        | Operator::I32GtU
        | Operator::I32LeS
        | Operator::I32LeU
        | Operator::I32GeS
        | Operator::I32GeU
        | Operator::I64Eqz
        | Operator::I64Eq
        | Operator::I64Ne
        | Operator::I64LtS
        | Operator::I64LtU
        | Operator::I64GtU
        | Operator::I64GtS
        | Operator::I64LeS
        | Operator::I64LeU
        | Operator::I64GeS
        | Operator::I64GeU
        | Operator::F32Eq
        | Operator::F32Ne
        | Operator::F32Lt
        | Operator::F32Gt
        | Operator::F32Le
        | Operator::F32Ge
        | Operator::F64Eq
        | Operator::F64Ne
        | Operator::F64Lt
        | Operator::F64Gt
        | Operator::F64Le
        | Operator::F64Ge => Ok(vec![]),

        Operator::I32Clz
        | Operator::I32Ctz
        | Operator::I32Popcnt
        | Operator::I32Add
        | Operator::I32Sub
        | Operator::I32Mul
        | Operator::I32And
        | Operator::I32Or
        | Operator::I32Xor
        | Operator::I32Shl
        | Operator::I32ShrS
        | Operator::I32ShrU
        | Operator::I32Rotl
        | Operator::I32Rotr => Ok(vec![]),

        Operator::I32DivS | Operator::I32DivU | Operator::I32RemS | Operator::I32RemU => {
            Ok(vec![Trap])
        }

        Operator::I64Clz
        | Operator::I64Ctz
        | Operator::I64Popcnt
        | Operator::I64Add
        | Operator::I64Sub
        | Operator::I64Mul
        | Operator::I64And
        | Operator::I64Or
        | Operator::I64Xor
        | Operator::I64Shl
        | Operator::I64ShrS
        | Operator::I64ShrU
        | Operator::I64Rotl
        | Operator::I64Rotr => Ok(vec![]),

        Operator::I64DivS | Operator::I64DivU | Operator::I64RemS | Operator::I64RemU => {
            Ok(vec![Trap])
        }

        Operator::F32Abs
        | Operator::F32Neg
        | Operator::F32Ceil
        | Operator::F32Floor
        | Operator::F32Trunc
        | Operator::F32Nearest
        | Operator::F32Sqrt
        | Operator::F32Add
        | Operator::F32Sub
        | Operator::F32Mul
        | Operator::F32Div
        | Operator::F32Min
        | Operator::F32Max
        | Operator::F32Copysign => Ok(vec![]),

        Operator::F64Abs
        | Operator::F64Neg
        | Operator::F64Ceil
        | Operator::F64Floor
        | Operator::F64Trunc
        | Operator::F64Nearest
        | Operator::F64Sqrt
        | Operator::F64Add
        | Operator::F64Sub
        | Operator::F64Mul
        | Operator::F64Div
        | Operator::F64Min
        | Operator::F64Max
        | Operator::F64Copysign => Ok(vec![]),

        Operator::I32WrapI64 => Ok(vec![]),
        Operator::I32TruncF32S => Ok(vec![Trap]),
        Operator::I32TruncF32U => Ok(vec![Trap]),
        Operator::I32TruncF64S => Ok(vec![Trap]),
        Operator::I32TruncF64U => Ok(vec![Trap]),
        Operator::I64ExtendI32S => Ok(vec![]),
        Operator::I64ExtendI32U => Ok(vec![]),
        Operator::I64TruncF32S => Ok(vec![Trap]),
        Operator::I64TruncF32U => Ok(vec![Trap]),
        Operator::I64TruncF64S => Ok(vec![Trap]),
        Operator::I64TruncF64U => Ok(vec![Trap]),
        Operator::F32ConvertI32S => Ok(vec![]),
        Operator::F32ConvertI32U => Ok(vec![]),
        Operator::F32ConvertI64S => Ok(vec![]),
        Operator::F32ConvertI64U => Ok(vec![]),
        Operator::F32DemoteF64 => Ok(vec![]),
        Operator::F64ConvertI32S => Ok(vec![]),
        Operator::F64ConvertI32U => Ok(vec![]),
        Operator::F64ConvertI64S => Ok(vec![]),
        Operator::F64ConvertI64U => Ok(vec![]),
        Operator::F64PromoteF32 => Ok(vec![]),
        Operator::I32Extend8S => Ok(vec![]),
        Operator::I32Extend16S => Ok(vec![]),
        Operator::I64Extend8S => Ok(vec![]),
        Operator::I64Extend16S => Ok(vec![]),
        Operator::I64Extend32S => Ok(vec![]),
        Operator::I32TruncSatF32S => Ok(vec![]),
        Operator::I32TruncSatF32U => Ok(vec![]),
        Operator::I32TruncSatF64S => Ok(vec![]),
        Operator::I32TruncSatF64U => Ok(vec![]),
        Operator::I64TruncSatF32S => Ok(vec![]),
        Operator::I64TruncSatF32U => Ok(vec![]),
        Operator::I64TruncSatF64S => Ok(vec![]),
        Operator::I64TruncSatF64U => Ok(vec![]),
        Operator::F32ReinterpretI32 => Ok(vec![]),
        Operator::F64ReinterpretI64 => Ok(vec![]),
        Operator::I32ReinterpretF32 => Ok(vec![]),
        Operator::I64ReinterpretF64 => Ok(vec![]),
        Operator::TableGet { table, .. } => Ok(vec![ReadTable(*table as usize), Trap]),
        Operator::TableSet { table, .. } => Ok(vec![WriteTable(*table as usize), Trap]),
        Operator::TableGrow { table, .. } => Ok(vec![WriteTable(*table as usize), Trap]),
        Operator::TableSize { table, .. } => Ok(vec![ReadTable(*table as usize)]),
        Operator::MemorySize { .. } => Ok(vec![ReadMem]),
        Operator::MemoryGrow { .. } => Ok(vec![WriteMem, Trap]),

        _ => bail!("Unknown operator in op_outputs(): {:?}", op),
    }
}
