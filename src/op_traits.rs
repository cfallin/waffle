//! Metadata on operators.

use crate::ir::{Module, Type, Value};
use crate::Operator;
use anyhow::Result;
use std::borrow::Cow;

pub fn op_inputs(
    module: &Module,
    op_stack: Option<&[(Type, Value)]>,
    op: &Operator,
) -> Result<Cow<'static, [Type]>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(Cow::Borrowed(&[])),

        &Operator::Call { function_index } => {
            let sig = module.funcs[function_index].sig();
            Ok(Vec::from(module.signatures[sig].params.clone()).into())
        }
        &Operator::CallIndirect { sig_index, .. } => {
            let mut params = module.signatures[sig_index].params.to_vec();
            params.push(Type::I32);
            Ok(params.into())
        }

        &Operator::Select => {
            let Some(op_stack) = op_stack else{
                anyhow::bail!("selects cannot be typed with no stack");
            };
            let val_ty = op_stack[op_stack.len() - 2].0;
            Ok(vec![val_ty, val_ty, Type::I32].into())
        }
        &Operator::TypedSelect { ty } => Ok(vec![ty, ty, Type::I32].into()),

        &Operator::GlobalGet { .. } => Ok(Cow::Borrowed(&[])),
        &Operator::GlobalSet { global_index } => Ok(vec![module.globals[global_index].ty].into()),

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
        | Operator::I64Load32U { .. } => Ok(Cow::Borrowed(&[Type::I32])),

        Operator::I32Store { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::I32])),
        Operator::I64Store { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::I64])),
        Operator::F32Store { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::F32])),
        Operator::F64Store { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::F64])),
        Operator::I32Store8 { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::I32])),
        Operator::I32Store16 { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::I32])),
        Operator::I64Store8 { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::I64])),
        Operator::I64Store16 { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::I64])),
        Operator::I64Store32 { .. } => Ok(Cow::Borrowed(&[Type::I32, Type::I64])),

        Operator::I32Const { .. }
        | Operator::I64Const { .. }
        | Operator::F32Const { .. }
        | Operator::F64Const { .. } => Ok(Cow::Borrowed(&[])),

        Operator::I32Eqz => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32Eq
        | Operator::I32Ne
        | Operator::I32LtS
        | Operator::I32LtU
        | Operator::I32GtS
        | Operator::I32GtU
        | Operator::I32LeS
        | Operator::I32LeU
        | Operator::I32GeS
        | Operator::I32GeU => Ok(Cow::Borrowed(&[Type::I32, Type::I32])),

        Operator::I64Eqz => Ok(Cow::Borrowed(&[Type::I64])),

        Operator::I64Eq
        | Operator::I64Ne
        | Operator::I64LtS
        | Operator::I64LtU
        | Operator::I64GtU
        | Operator::I64GtS
        | Operator::I64LeS
        | Operator::I64LeU
        | Operator::I64GeS
        | Operator::I64GeU => Ok(Cow::Borrowed(&[Type::I64, Type::I64])),

        Operator::F32Eq
        | Operator::F32Ne
        | Operator::F32Lt
        | Operator::F32Gt
        | Operator::F32Le
        | Operator::F32Ge => Ok(Cow::Borrowed(&[Type::F32, Type::F32])),

        Operator::F64Eq
        | Operator::F64Ne
        | Operator::F64Lt
        | Operator::F64Gt
        | Operator::F64Le
        | Operator::F64Ge => Ok(Cow::Borrowed(&[Type::F64, Type::F64])),

        Operator::I32Clz | Operator::I32Ctz | Operator::I32Popcnt => {
            Ok(Cow::Borrowed(&[Type::I32]))
        }

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
        | Operator::I32Rotr => Ok(Cow::Borrowed(&[Type::I32, Type::I32])),

        Operator::I64Clz | Operator::I64Ctz | Operator::I64Popcnt => {
            Ok(Cow::Borrowed(&[Type::I64]))
        }

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
        | Operator::I64Rotr => Ok(Cow::Borrowed(&[Type::I64, Type::I64])),

        Operator::F32Abs
        | Operator::F32Neg
        | Operator::F32Ceil
        | Operator::F32Floor
        | Operator::F32Trunc
        | Operator::F32Nearest
        | Operator::F32Sqrt => Ok(Cow::Borrowed(&[Type::F32])),

        Operator::F32Add
        | Operator::F32Sub
        | Operator::F32Mul
        | Operator::F32Div
        | Operator::F32Min
        | Operator::F32Max
        | Operator::F32Copysign => Ok(Cow::Borrowed(&[Type::F32, Type::F32])),

        Operator::F64Abs
        | Operator::F64Neg
        | Operator::F64Ceil
        | Operator::F64Floor
        | Operator::F64Trunc
        | Operator::F64Nearest
        | Operator::F64Sqrt => Ok(Cow::Borrowed(&[Type::F64])),

        Operator::F64Add
        | Operator::F64Sub
        | Operator::F64Mul
        | Operator::F64Div
        | Operator::F64Min
        | Operator::F64Max
        | Operator::F64Copysign => Ok(Cow::Borrowed(&[Type::F64, Type::F64])),

        Operator::I32WrapI64 => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I32TruncF32S => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I32TruncF32U => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I32TruncF64S => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I32TruncF64U => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I64ExtendI32S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64ExtendI32U => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64TruncF32S => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I64TruncF32U => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I64TruncF64S => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I64TruncF64U => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::F32ConvertI32S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::F32ConvertI32U => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::F32ConvertI64S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F32ConvertI64U => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F32DemoteF64 => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::F64ConvertI32S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::F64ConvertI32U => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::F64ConvertI64S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F64ConvertI64U => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F64PromoteF32 => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I32Extend8S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32Extend16S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64Extend8S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64Extend16S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64Extend32S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I32TruncSatF32S => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I32TruncSatF32U => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I32TruncSatF64S => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I32TruncSatF64U => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I64TruncSatF32S => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I64TruncSatF32U => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I64TruncSatF64S => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I64TruncSatF64U => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::F32ReinterpretI32 => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::F64ReinterpretI64 => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I32ReinterpretF32 => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::I64ReinterpretF64 => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::TableGet { .. } => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::TableSet { table_index } => {
            Ok(vec![Type::I32, module.tables[*table_index].ty].into())
        }
        Operator::TableGrow { table_index } => {
            Ok(vec![Type::I32, module.tables[*table_index].ty].into())
        }
        Operator::TableSize { .. } => Ok(Cow::Borrowed(&[])),
        Operator::MemorySize { .. } => Ok(Cow::Borrowed(&[])),
        Operator::MemoryGrow { .. } => Ok(Cow::Borrowed(&[Type::I32])),
    }
}

pub fn op_outputs(
    module: &Module,
    op_stack: Option<&[(Type, Value)]>,
    op: &Operator,
) -> Result<Cow<'static, [Type]>> {
    match op {
        &Operator::Unreachable | &Operator::Nop => Ok(Cow::Borrowed(&[])),

        &Operator::Call { function_index } => {
            let sig = module.funcs[function_index].sig();
            Ok(Vec::from(module.signatures[sig].returns.clone()).into())
        }
        &Operator::CallIndirect { sig_index, .. } => {
            Ok(Vec::from(module.signatures[sig_index].returns.clone()).into())
        }

        &Operator::Select => {
            let Some(op_stack) = op_stack else{
                anyhow::bail!("selects cannot be typed with no stack");
            };
            let val_ty = op_stack[op_stack.len() - 2].0;
            Ok(vec![val_ty].into())
        }
        &Operator::TypedSelect { ty } => Ok(vec![ty].into()),
        &Operator::GlobalGet { global_index } => Ok(vec![module.globals[global_index].ty].into()),
        &Operator::GlobalSet { .. } => Ok(Cow::Borrowed(&[])),

        Operator::I32Load { .. }
        | Operator::I32Load8S { .. }
        | Operator::I32Load8U { .. }
        | Operator::I32Load16S { .. }
        | Operator::I32Load16U { .. } => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64Load { .. }
        | Operator::I64Load8S { .. }
        | Operator::I64Load8U { .. }
        | Operator::I64Load16S { .. }
        | Operator::I64Load16U { .. }
        | Operator::I64Load32S { .. }
        | Operator::I64Load32U { .. } => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F32Load { .. } => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F64Load { .. } => Ok(Cow::Borrowed(&[Type::F64])),

        Operator::I32Store { .. } => Ok(Cow::Borrowed(&[])),
        Operator::I64Store { .. } => Ok(Cow::Borrowed(&[])),
        Operator::F32Store { .. } => Ok(Cow::Borrowed(&[])),
        Operator::F64Store { .. } => Ok(Cow::Borrowed(&[])),
        Operator::I32Store8 { .. } => Ok(Cow::Borrowed(&[])),
        Operator::I32Store16 { .. } => Ok(Cow::Borrowed(&[])),
        Operator::I64Store8 { .. } => Ok(Cow::Borrowed(&[])),
        Operator::I64Store16 { .. } => Ok(Cow::Borrowed(&[])),
        Operator::I64Store32 { .. } => Ok(Cow::Borrowed(&[])),

        Operator::I32Const { .. } => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64Const { .. } => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F32Const { .. } => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F64Const { .. } => Ok(Cow::Borrowed(&[Type::F64])),

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
        | Operator::F64Ge => Ok(Cow::Borrowed(&[Type::I32])),

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
        | Operator::I32Rotr => Ok(Cow::Borrowed(&[Type::I32])),

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
        | Operator::I64Rotr => Ok(Cow::Borrowed(&[Type::I64])),

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
        | Operator::F32Copysign => Ok(Cow::Borrowed(&[Type::F32])),

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
        | Operator::F64Copysign => Ok(Cow::Borrowed(&[Type::F64])),

        Operator::I32WrapI64 => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32TruncF32S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32TruncF32U => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32TruncF64S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32TruncF64U => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64ExtendI32S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64ExtendI32U => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64TruncF32S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64TruncF32U => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64TruncF64S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64TruncF64U => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F32ConvertI32S => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F32ConvertI32U => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F32ConvertI64S => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F32ConvertI64U => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F32DemoteF64 => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F64ConvertI32S => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::F64ConvertI32U => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::F64ConvertI64S => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::F64ConvertI64U => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::F64PromoteF32 => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I32Extend8S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32Extend16S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64Extend8S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64Extend16S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64Extend32S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I32TruncSatF32S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32TruncSatF32U => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32TruncSatF64S => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I32TruncSatF64U => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64TruncSatF32S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64TruncSatF32U => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64TruncSatF64S => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::I64TruncSatF64U => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::F32ReinterpretI32 => Ok(Cow::Borrowed(&[Type::F32])),
        Operator::F64ReinterpretI64 => Ok(Cow::Borrowed(&[Type::F64])),
        Operator::I32ReinterpretF32 => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::I64ReinterpretF64 => Ok(Cow::Borrowed(&[Type::I64])),
        Operator::TableGet { table_index } => Ok(vec![module.tables[*table_index].ty].into()),
        Operator::TableSet { .. } => Ok(Cow::Borrowed(&[])),
        Operator::TableGrow { .. } => Ok(Cow::Borrowed(&[])),
        Operator::TableSize { .. } => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::MemorySize { .. } => Ok(Cow::Borrowed(&[Type::I32])),
        Operator::MemoryGrow { .. } => Ok(Cow::Borrowed(&[Type::I32])),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SideEffect {
    Trap,
    ReadMem,
    WriteMem,
    ReadGlobal,
    WriteGlobal,
    ReadTable,
    WriteTable,
    ReadLocal,
    WriteLocal,
    All,
}

impl Operator {
    pub fn effects(&self) -> &'static [SideEffect] {
        use SideEffect::*;

        match self {
            &Operator::Unreachable => &[Trap],
            &Operator::Nop => &[],

            &Operator::Call { .. } => &[All],
            &Operator::CallIndirect { .. } => &[All],

            &Operator::Select => &[],
            &Operator::TypedSelect { .. } => &[],
            &Operator::GlobalGet { .. } => &[ReadGlobal],
            &Operator::GlobalSet { .. } => &[WriteGlobal],

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
            | Operator::F64Load { .. } => &[Trap, ReadMem],

            Operator::I32Store { .. }
            | Operator::I64Store { .. }
            | Operator::F32Store { .. }
            | Operator::F64Store { .. }
            | Operator::I32Store8 { .. }
            | Operator::I32Store16 { .. }
            | Operator::I64Store8 { .. }
            | Operator::I64Store16 { .. }
            | Operator::I64Store32 { .. } => &[Trap, WriteMem],

            Operator::I32Const { .. }
            | Operator::I64Const { .. }
            | Operator::F32Const { .. }
            | Operator::F64Const { .. } => &[],

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
            | Operator::F64Ge => &[],

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
            | Operator::I32Rotr => &[],

            Operator::I32DivS | Operator::I32DivU | Operator::I32RemS | Operator::I32RemU => {
                &[Trap]
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
            | Operator::I64Rotr => &[],

            Operator::I64DivS | Operator::I64DivU | Operator::I64RemS | Operator::I64RemU => {
                &[Trap]
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
            | Operator::F32Copysign => &[],

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
            | Operator::F64Copysign => &[],

            Operator::I32WrapI64 => &[],
            Operator::I32TruncF32S => &[Trap],
            Operator::I32TruncF32U => &[Trap],
            Operator::I32TruncF64S => &[Trap],
            Operator::I32TruncF64U => &[Trap],
            Operator::I64ExtendI32S => &[],
            Operator::I64ExtendI32U => &[],
            Operator::I64TruncF32S => &[Trap],
            Operator::I64TruncF32U => &[Trap],
            Operator::I64TruncF64S => &[Trap],
            Operator::I64TruncF64U => &[Trap],
            Operator::F32ConvertI32S => &[],
            Operator::F32ConvertI32U => &[],
            Operator::F32ConvertI64S => &[],
            Operator::F32ConvertI64U => &[],
            Operator::F32DemoteF64 => &[],
            Operator::F64ConvertI32S => &[],
            Operator::F64ConvertI32U => &[],
            Operator::F64ConvertI64S => &[],
            Operator::F64ConvertI64U => &[],
            Operator::F64PromoteF32 => &[],
            Operator::I32Extend8S => &[],
            Operator::I32Extend16S => &[],
            Operator::I64Extend8S => &[],
            Operator::I64Extend16S => &[],
            Operator::I64Extend32S => &[],
            Operator::I32TruncSatF32S => &[],
            Operator::I32TruncSatF32U => &[],
            Operator::I32TruncSatF64S => &[],
            Operator::I32TruncSatF64U => &[],
            Operator::I64TruncSatF32S => &[],
            Operator::I64TruncSatF32U => &[],
            Operator::I64TruncSatF64S => &[],
            Operator::I64TruncSatF64U => &[],
            Operator::F32ReinterpretI32 => &[],
            Operator::F64ReinterpretI64 => &[],
            Operator::I32ReinterpretF32 => &[],
            Operator::I64ReinterpretF64 => &[],
            Operator::TableGet { .. } => &[ReadTable, Trap],
            Operator::TableSet { .. } => &[WriteTable, Trap],
            Operator::TableGrow { .. } => &[WriteTable, Trap],
            Operator::TableSize { .. } => &[ReadTable],
            Operator::MemorySize { .. } => &[ReadMem],
            Operator::MemoryGrow { .. } => &[WriteMem, Trap],
        }
    }

    pub fn is_pure(&self) -> bool {
        self.effects().is_empty()
    }

    pub fn is_call(&self) -> bool {
        match self {
            Operator::Call { .. } | Operator::CallIndirect { .. } => true,
            _ => false,
        }
    }

    pub fn accesses_memory(&self) -> bool {
        self.effects().iter().any(|e| match e {
            SideEffect::ReadMem | SideEffect::WriteMem => true,
            _ => false,
        })
    }
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &Operator::Unreachable => write!(f, "unreachable")?,
            &Operator::Nop => write!(f, "nop")?,

            &Operator::Call { function_index } => write!(f, "call<{}>", function_index)?,
            &Operator::CallIndirect {
                sig_index,
                table_index,
            } => write!(f, "call_indirect<{}, {}>", sig_index, table_index)?,

            &Operator::Select => write!(f, "select")?,
            &Operator::TypedSelect { ty } => write!(f, "typed_select<{}>", ty)?,
            &Operator::GlobalGet { global_index, .. } => write!(f, "global_get<{}>", global_index)?,
            &Operator::GlobalSet { global_index, .. } => write!(f, "global_set<{}>", global_index)?,

            Operator::I32Load { memory } => write!(f, "i32load<{}>", memory)?,
            Operator::I32Load8S { memory } => write!(f, "i32load8s<{}>", memory)?,
            Operator::I32Load8U { memory } => write!(f, "i32load8u<{}>", memory)?,
            Operator::I32Load16S { memory } => write!(f, "i32load16s<{}>", memory)?,
            Operator::I32Load16U { memory } => write!(f, "i32load16u<{}>", memory)?,
            Operator::I64Load { memory } => write!(f, "i64load<{}>", memory)?,
            Operator::I64Load8S { memory } => write!(f, "i64load8s<{}>", memory)?,
            Operator::I64Load8U { memory } => write!(f, "i64load8u<{}>", memory)?,
            Operator::I64Load16S { memory } => write!(f, "i64load16s<{}>", memory)?,
            Operator::I64Load16U { memory } => write!(f, "i64load16u<{}>", memory)?,
            Operator::I64Load32S { memory } => write!(f, "i64load32s<{}>", memory)?,
            Operator::I64Load32U { memory } => write!(f, "i64load32u<{}>", memory)?,
            Operator::F32Load { memory } => write!(f, "f32load<{}>", memory)?,
            Operator::F64Load { memory } => write!(f, "f64load<{}>", memory)?,

            Operator::I32Store { memory } => write!(f, "i32store<{}>", memory)?,
            Operator::I64Store { memory } => write!(f, "i64store<{}>", memory)?,
            Operator::F32Store { memory } => write!(f, "f32store<{}>", memory)?,
            Operator::F64Store { memory } => write!(f, "f64store<{}>", memory)?,
            Operator::I32Store8 { memory } => write!(f, "i32store8<{}>", memory)?,
            Operator::I32Store16 { memory } => write!(f, "i32store16<{}>", memory)?,
            Operator::I64Store8 { memory } => write!(f, "i64store8<{}>", memory)?,
            Operator::I64Store16 { memory } => write!(f, "i64store16<{}>", memory)?,
            Operator::I64Store32 { memory } => write!(f, "i64store32<{}>", memory)?,

            Operator::I32Const { value } => write!(f, "i32const<{}>", value)?,
            Operator::I64Const { value } => write!(f, "i64const<{}>", value)?,
            Operator::F32Const { value } => write!(f, "f32const<{}>", value)?,
            Operator::F64Const { value } => write!(f, "f64const<{}>", value)?,

            Operator::I32Eqz => write!(f, "i32eqz")?,
            Operator::I32Eq => write!(f, "i32eq")?,
            Operator::I32Ne => write!(f, "i32ne")?,
            Operator::I32LtS => write!(f, "i32lts")?,
            Operator::I32LtU => write!(f, "i32ltu")?,
            Operator::I32GtS => write!(f, "i32gts")?,
            Operator::I32GtU => write!(f, "i32gtu")?,
            Operator::I32LeS => write!(f, "i32les")?,
            Operator::I32LeU => write!(f, "i32leu")?,
            Operator::I32GeS => write!(f, "i64ges")?,
            Operator::I32GeU => write!(f, "i32geu")?,
            Operator::I64Eqz => write!(f, "i64eqz")?,
            Operator::I64Eq => write!(f, "i64eq")?,
            Operator::I64Ne => write!(f, "i64ne")?,
            Operator::I64LtS => write!(f, "i64lts")?,
            Operator::I64LtU => write!(f, "i64ltu")?,
            Operator::I64GtU => write!(f, "i64gtu")?,
            Operator::I64GtS => write!(f, "i64gts")?,
            Operator::I64LeS => write!(f, "i64les")?,
            Operator::I64LeU => write!(f, "i64leu")?,
            Operator::I64GeS => write!(f, "i64ges")?,
            Operator::I64GeU => write!(f, "i64geu")?,
            Operator::F32Eq => write!(f, "f32eq")?,
            Operator::F32Ne => write!(f, "f32ne")?,
            Operator::F32Lt => write!(f, "f32lt")?,
            Operator::F32Gt => write!(f, "f32gt")?,
            Operator::F32Le => write!(f, "f32le")?,
            Operator::F32Ge => write!(f, "f32ge")?,
            Operator::F64Eq => write!(f, "f64eq")?,
            Operator::F64Ne => write!(f, "f64ne")?,
            Operator::F64Lt => write!(f, "f64lt")?,
            Operator::F64Gt => write!(f, "f64gt")?,
            Operator::F64Le => write!(f, "f64le")?,
            Operator::F64Ge => write!(f, "f64ge")?,

            Operator::I32Clz => write!(f, "i32clz")?,
            Operator::I32Ctz => write!(f, "i32ctz")?,
            Operator::I32Popcnt => write!(f, "i32popcnt")?,
            Operator::I32Add => write!(f, "i32add")?,
            Operator::I32Sub => write!(f, "i32sub")?,
            Operator::I32Mul => write!(f, "i32mul")?,
            Operator::I32And => write!(f, "i32and")?,
            Operator::I32Or => write!(f, "i32or")?,
            Operator::I32Xor => write!(f, "i32xor")?,
            Operator::I32Shl => write!(f, "i32shl")?,
            Operator::I32ShrS => write!(f, "i32shrs")?,
            Operator::I32ShrU => write!(f, "i32shru")?,
            Operator::I32Rotl => write!(f, "i32rotl")?,
            Operator::I32Rotr => write!(f, "i32rotr")?,

            Operator::I32DivS => write!(f, "i32divs")?,
            Operator::I32DivU => write!(f, "i32divu")?,
            Operator::I32RemS => write!(f, "i32rems")?,
            Operator::I32RemU => write!(f, "i32remu")?,

            Operator::I64Clz => write!(f, "i64clz")?,
            Operator::I64Ctz => write!(f, "i64ctz")?,
            Operator::I64Popcnt => write!(f, "i64popcnt")?,
            Operator::I64Add => write!(f, "i64add")?,
            Operator::I64Sub => write!(f, "i64sub")?,
            Operator::I64Mul => write!(f, "i64mul")?,
            Operator::I64And => write!(f, "i64and")?,
            Operator::I64Or => write!(f, "i64or")?,
            Operator::I64Xor => write!(f, "i64xor")?,
            Operator::I64Shl => write!(f, "i64shl")?,
            Operator::I64ShrS => write!(f, "i64shrs")?,
            Operator::I64ShrU => write!(f, "i64shru")?,
            Operator::I64Rotl => write!(f, "i64rotl")?,
            Operator::I64Rotr => write!(f, "i64rotr")?,

            Operator::I64DivS => write!(f, "i64divs")?,
            Operator::I64DivU => write!(f, "i64divu")?,
            Operator::I64RemS => write!(f, "i64rems")?,
            Operator::I64RemU => write!(f, "i64remu")?,

            Operator::F32Abs => write!(f, "f32abs")?,
            Operator::F32Neg => write!(f, "f32neg")?,
            Operator::F32Ceil => write!(f, "f32ceil")?,
            Operator::F32Floor => write!(f, "f32floor")?,
            Operator::F32Trunc => write!(f, "f32trunc")?,
            Operator::F32Nearest => write!(f, "f32nearest")?,
            Operator::F32Sqrt => write!(f, "f32sqrt")?,
            Operator::F32Add => write!(f, "f32add")?,
            Operator::F32Sub => write!(f, "f32sub")?,
            Operator::F32Mul => write!(f, "f32mul")?,
            Operator::F32Div => write!(f, "f32div")?,
            Operator::F32Min => write!(f, "f32min")?,
            Operator::F32Max => write!(f, "f32max")?,
            Operator::F32Copysign => write!(f, "f32copysign")?,

            Operator::F64Abs => write!(f, "f64abs")?,
            Operator::F64Neg => write!(f, "f64neg")?,
            Operator::F64Ceil => write!(f, "f64ceil")?,
            Operator::F64Floor => write!(f, "f64flor")?,
            Operator::F64Trunc => write!(f, "f64trunc")?,
            Operator::F64Nearest => write!(f, "f64nearest")?,
            Operator::F64Sqrt => write!(f, "f64sqrt")?,
            Operator::F64Add => write!(f, "f64add")?,
            Operator::F64Sub => write!(f, "f64sub")?,
            Operator::F64Mul => write!(f, "f64mul")?,
            Operator::F64Div => write!(f, "f64div")?,
            Operator::F64Min => write!(f, "f64min")?,
            Operator::F64Max => write!(f, "f64max")?,
            Operator::F64Copysign => write!(f, "f64copysign")?,

            Operator::I32WrapI64 => write!(f, "i32wrapi64")?,
            Operator::I32TruncF32S => write!(f, "i32truncf32s")?,
            Operator::I32TruncF32U => write!(f, "i32truncf32u")?,
            Operator::I32TruncF64S => write!(f, "i32truncf64s")?,
            Operator::I32TruncF64U => write!(f, "i32truncf64u")?,
            Operator::I64ExtendI32S => write!(f, "i64extendi32s")?,
            Operator::I64ExtendI32U => write!(f, "i64extendi32u")?,
            Operator::I64TruncF32S => write!(f, "i64truncf32s")?,
            Operator::I64TruncF32U => write!(f, "i64truncf32u")?,
            Operator::I64TruncF64S => write!(f, "i64truncf64s")?,
            Operator::I64TruncF64U => write!(f, "i64truncf64u")?,
            Operator::F32ConvertI32S => write!(f, "f32converti32s")?,
            Operator::F32ConvertI32U => write!(f, "f32converti32u")?,
            Operator::F32ConvertI64S => write!(f, "f32converti64s")?,
            Operator::F32ConvertI64U => write!(f, "f32converti64u")?,
            Operator::F32DemoteF64 => write!(f, "f32demotef64")?,
            Operator::F64ConvertI32S => write!(f, "f64converti32s")?,
            Operator::F64ConvertI32U => write!(f, "f64converti32u")?,
            Operator::F64ConvertI64S => write!(f, "f64converti64s")?,
            Operator::F64ConvertI64U => write!(f, "f64converti64u")?,
            Operator::F64PromoteF32 => write!(f, "f64promotef32")?,
            Operator::I32Extend8S => write!(f, "i32extend8s")?,
            Operator::I32Extend16S => write!(f, "i32extend16s")?,
            Operator::I64Extend8S => write!(f, "i64extend8s")?,
            Operator::I64Extend16S => write!(f, "i64extend16s")?,
            Operator::I64Extend32S => write!(f, "i64extend32s")?,
            Operator::I32TruncSatF32S => write!(f, "i32truncsatf32s")?,
            Operator::I32TruncSatF32U => write!(f, "i32truncsatf32u")?,
            Operator::I32TruncSatF64S => write!(f, "i32truncsatf64s")?,
            Operator::I32TruncSatF64U => write!(f, "i32truncsatf64u")?,
            Operator::I64TruncSatF32S => write!(f, "i64truncsatf32s")?,
            Operator::I64TruncSatF32U => write!(f, "i64truncsatf32u")?,
            Operator::I64TruncSatF64S => write!(f, "i64truncsatf64s")?,
            Operator::I64TruncSatF64U => write!(f, "i64truncsatf64u")?,
            Operator::F32ReinterpretI32 => write!(f, "f32reinterpreti32")?,
            Operator::F64ReinterpretI64 => write!(f, "f64reinterpreti64")?,
            Operator::I32ReinterpretF32 => write!(f, "i32reinterpretf32")?,
            Operator::I64ReinterpretF64 => write!(f, "i64reinterpretf64")?,
            Operator::TableGet { table_index, .. } => write!(f, "table_get<{}>", table_index)?,
            Operator::TableSet { table_index, .. } => write!(f, "table_set<{}>", table_index)?,
            Operator::TableGrow { table_index, .. } => write!(f, "table_grow<{}>", table_index)?,
            Operator::TableSize { table_index, .. } => write!(f, "table_size<{}>", table_index)?,
            Operator::MemorySize { mem } => write!(f, "memory_size<{}>", mem)?,
            Operator::MemoryGrow { mem } => write!(f, "memory_grow<{}>", mem)?,
        }

        Ok(())
    }
}

pub fn op_rematerialize(op: &Operator) -> bool {
    match op {
        &Operator::I32Const { .. }
        | &Operator::I64Const { .. }
        | &Operator::F32Const { .. }
        | &Operator::F64Const { .. } => true,
        _ => false,
    }
}
