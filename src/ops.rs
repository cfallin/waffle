//! Operators.

use wasmparser::{Ieee32, Ieee64, MemoryImmediate, Type};

use crate::{FuncId, GlobalId, LocalId, MemoryId, TableId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Memory {
    pub align: u8,
    pub offset: u64,
    pub memory: MemoryId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    Unreachable,
    Nop,

    Call { function_index: FuncId },
    CallIndirect { index: FuncId, table_index: TableId },
    Return,
    LocalSet { local_index: LocalId },
    LocalTee { local_index: LocalId },
    LocalGet { local_index: LocalId },
    Select,
    TypedSelect { ty: Type },
    GlobalGet { global_index: GlobalId },
    GlobalSet { global_index: GlobalId },

    I32Load { memory: Memory },
    I64Load { memory: Memory },
    F32Load { memory: Memory },
    F64Load { memory: Memory },
    I32Load8S { memory: Memory },
    I32Load8U { memory: Memory },
    I32Load16S { memory: Memory },
    I32Load16U { memory: Memory },
    I64Load8S { memory: Memory },
    I64Load8U { memory: Memory },
    I64Load16S { memory: Memory },
    I64Load16U { memory: Memory },
    I64Load32S { memory: Memory },
    I64Load32U { memory: Memory },

    I32Store { memory: Memory },
    I64Store { memory: Memory },
    F32Store { memory: Memory },
    F64Store { memory: Memory },
    I32Store8 { memory: Memory },
    I32Store16 { memory: Memory },
    I64Store8 { memory: Memory },
    I64Store16 { memory: Memory },
    I64Store32 { memory: Memory },

    I32Const { value: i32 },
    I64Const { value: i64 },
    F32Const { value: Ieee32 },
    F64Const { value: Ieee64 },

    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,

    I64Eqz,

    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtU,
    I64GtS,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,

    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,

    I32Clz,
    I32Ctz,
    I32Popcnt,

    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,

    I64Clz,
    I64Ctz,
    I64Popcnt,

    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,

    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,

    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,

    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,

    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,

    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,
    I32TruncSatF32S,
    I32TruncSatF32U,
    I32TruncSatF64S,
    I32TruncSatF64U,
    I64TruncSatF32S,
    I64TruncSatF32U,
    I64TruncSatF64S,
    I64TruncSatF64U,
    F32ReinterpretI32,
    F64ReinterpretI64,
    I32ReinterpretF32,
    I64ReinterpretF64,
    TableGet { table: TableId },
    TableSet { table: TableId },
    TableGrow { table: TableId },
    TableSize { table: TableId },
    MemorySize { mem: MemoryId },
    MemoryGrow { mem: MemoryId },
}

impl<'a, 'b> std::convert::TryFrom<&'b wasmparser::Operator<'a>> for Operator {
    type Error = ();

    fn try_from(op: &'b wasmparser::Operator<'a>) -> Result<Operator, Self::Error> {
        match op {
            &wasmparser::Operator::Unreachable => Ok(Operator::Unreachable),
            &wasmparser::Operator::Nop => Ok(Operator::Nop),
            &wasmparser::Operator::Call { function_index } => Ok(Operator::Call {
                function_index: function_index as usize,
            }),
            &wasmparser::Operator::CallIndirect { index, table_index } => {
                Ok(Operator::CallIndirect {
                    index: index as usize,
                    table_index,
                })
            }
            &wasmparser::Operator::Return => Ok(Operator::Return),
            &wasmparser::Operator::LocalSet { local_index } => {
                Ok(Operator::LocalSet { local_index })
            }
            &wasmparser::Operator::LocalTee { local_index } => {
                Ok(Operator::LocalTee { local_index })
            }
            &wasmparser::Operator::LocalGet { local_index } => {
                Ok(Operator::LocalGet { local_index })
            }
            &wasmparser::Operator::Select => Ok(Operator::Select),
            &wasmparser::Operator::TypedSelect { ty } => Ok(Operator::TypedSelect { ty }),
            &wasmparser::Operator::GlobalGet { global_index } => {
                Ok(Operator::GlobalGet { global_index })
            }
            &wasmparser::Operator::GlobalSet { global_index } => {
                Ok(Operator::GlobalSet { global_index })
            }
            &wasmparser::Operator::I32Load { memarg } => Ok(Operator::I32Load {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Load { memarg } => Ok(Operator::I64Load {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::F32Load { memarg } => Ok(Operator::F32Load {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::F64Load { memarg } => Ok(Operator::F64Load {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Load8S { memarg } => Ok(Operator::I32Load8S {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Load8U { memarg } => Ok(Operator::I32Load8U {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Load16S { memarg } => Ok(Operator::I32Load16S {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Load16U { memarg } => Ok(Operator::I32Load16U {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Load8S { memarg } => Ok(Operator::I64Load8S {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Load8U { memarg } => Ok(Operator::I64Load8U {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Load16S { memarg } => Ok(Operator::I64Load16S {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Load16U { memarg } => Ok(Operator::I64Load16U {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Load32S { memarg } => Ok(Operator::I64Load32S {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Load32U { memarg } => Ok(Operator::I64Load32U {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Store { memarg } => Ok(Operator::I32Store {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Store { memarg } => Ok(Operator::I64Store {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::F32Store { memarg } => Ok(Operator::F32Store {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::F64Store { memarg } => Ok(Operator::F64Store {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Store8 { memarg } => Ok(Operator::I32Store8 {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Store16 { memarg } => Ok(Operator::I32Store16 {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Store8 { memarg } => Ok(Operator::I64Store8 {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Store16 { memarg } => Ok(Operator::I64Store16 {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I64Store32 { memarg } => Ok(Operator::I64Store32 {
                memory: memarg.into(),
            }),
            &wasmparser::Operator::I32Const { value } => Ok(Operator::I32Const { value }),
            &wasmparser::Operator::I64Const { value } => Ok(Operator::I64Const { value }),
            &wasmparser::Operator::F32Const { value } => Ok(Operator::F32Const { value }),
            &wasmparser::Operator::F64Const { value } => Ok(Operator::F64Const { value }),
            &wasmparser::Operator::I32Eqz => Ok(Operator::I32Eqz),
            &wasmparser::Operator::I32Eq => Ok(Operator::I32Eq),
            &wasmparser::Operator::I32Ne => Ok(Operator::I32Ne),
            &wasmparser::Operator::I32LtS => Ok(Operator::I32LtS),
            &wasmparser::Operator::I32LtU => Ok(Operator::I32LtU),
            &wasmparser::Operator::I32GtS => Ok(Operator::I32GtS),
            &wasmparser::Operator::I32GtU => Ok(Operator::I32GtU),
            &wasmparser::Operator::I32LeS => Ok(Operator::I32LeS),
            &wasmparser::Operator::I32LeU => Ok(Operator::I32LeU),
            &wasmparser::Operator::I32GeS => Ok(Operator::I32GeS),
            &wasmparser::Operator::I32GeU => Ok(Operator::I32GeU),
            &wasmparser::Operator::I64Eqz => Ok(Operator::I64Eqz),
            &wasmparser::Operator::I64Eq => Ok(Operator::I64Eq),
            &wasmparser::Operator::I64Ne => Ok(Operator::I64Ne),
            &wasmparser::Operator::I64LtS => Ok(Operator::I64LtS),
            &wasmparser::Operator::I64LtU => Ok(Operator::I64LtU),
            &wasmparser::Operator::I64GtU => Ok(Operator::I64GtU),
            &wasmparser::Operator::I64GtS => Ok(Operator::I64GtS),
            &wasmparser::Operator::I64LeS => Ok(Operator::I64LeS),
            &wasmparser::Operator::I64LeU => Ok(Operator::I64LeU),
            &wasmparser::Operator::I64GeS => Ok(Operator::I64GeS),
            &wasmparser::Operator::I64GeU => Ok(Operator::I64GeU),
            &wasmparser::Operator::F32Eq => Ok(Operator::F32Eq),
            &wasmparser::Operator::F32Ne => Ok(Operator::F32Ne),
            &wasmparser::Operator::F32Lt => Ok(Operator::F32Lt),
            &wasmparser::Operator::F32Gt => Ok(Operator::F32Gt),
            &wasmparser::Operator::F32Le => Ok(Operator::F32Le),
            &wasmparser::Operator::F32Ge => Ok(Operator::F32Ge),
            &wasmparser::Operator::F64Eq => Ok(Operator::F64Eq),
            &wasmparser::Operator::F64Ne => Ok(Operator::F64Ne),
            &wasmparser::Operator::F64Lt => Ok(Operator::F64Lt),
            &wasmparser::Operator::F64Gt => Ok(Operator::F64Gt),
            &wasmparser::Operator::F64Le => Ok(Operator::F64Le),
            &wasmparser::Operator::F64Ge => Ok(Operator::F64Ge),
            &wasmparser::Operator::I32Clz => Ok(Operator::I32Clz),
            &wasmparser::Operator::I32Ctz => Ok(Operator::I32Ctz),
            &wasmparser::Operator::I32Popcnt => Ok(Operator::I32Popcnt),
            &wasmparser::Operator::I32Add => Ok(Operator::I32Add),
            &wasmparser::Operator::I32Sub => Ok(Operator::I32Sub),
            &wasmparser::Operator::I32Mul => Ok(Operator::I32Mul),
            &wasmparser::Operator::I32DivS => Ok(Operator::I32DivS),
            &wasmparser::Operator::I32DivU => Ok(Operator::I32DivU),
            &wasmparser::Operator::I32RemS => Ok(Operator::I32RemS),
            &wasmparser::Operator::I32RemU => Ok(Operator::I32RemU),
            &wasmparser::Operator::I32And => Ok(Operator::I32And),
            &wasmparser::Operator::I32Or => Ok(Operator::I32Or),
            &wasmparser::Operator::I32Xor => Ok(Operator::I32Xor),
            &wasmparser::Operator::I32Shl => Ok(Operator::I32Shl),
            &wasmparser::Operator::I32ShrS => Ok(Operator::I32ShrS),
            &wasmparser::Operator::I32ShrU => Ok(Operator::I32ShrU),
            &wasmparser::Operator::I32Rotl => Ok(Operator::I32Rotl),
            &wasmparser::Operator::I32Rotr => Ok(Operator::I32Rotr),
            &wasmparser::Operator::I64Clz => Ok(Operator::I64Clz),
            &wasmparser::Operator::I64Ctz => Ok(Operator::I64Ctz),
            &wasmparser::Operator::I64Popcnt => Ok(Operator::I64Popcnt),
            &wasmparser::Operator::I64Add => Ok(Operator::I64Add),
            &wasmparser::Operator::I64Sub => Ok(Operator::I64Sub),
            &wasmparser::Operator::I64Mul => Ok(Operator::I64Mul),
            &wasmparser::Operator::I64DivS => Ok(Operator::I64DivS),
            &wasmparser::Operator::I64DivU => Ok(Operator::I64DivU),
            &wasmparser::Operator::I64RemS => Ok(Operator::I64RemS),
            &wasmparser::Operator::I64RemU => Ok(Operator::I64RemU),
            &wasmparser::Operator::I64And => Ok(Operator::I64And),
            &wasmparser::Operator::I64Or => Ok(Operator::I64Or),
            &wasmparser::Operator::I64Xor => Ok(Operator::I64Xor),
            &wasmparser::Operator::I64Shl => Ok(Operator::I64Shl),
            &wasmparser::Operator::I64ShrS => Ok(Operator::I64ShrS),
            &wasmparser::Operator::I64ShrU => Ok(Operator::I64ShrU),
            &wasmparser::Operator::I64Rotl => Ok(Operator::I64Rotl),
            &wasmparser::Operator::I64Rotr => Ok(Operator::I64Rotr),
            &wasmparser::Operator::F32Abs => Ok(Operator::F32Abs),
            &wasmparser::Operator::F32Neg => Ok(Operator::F32Neg),
            &wasmparser::Operator::F32Ceil => Ok(Operator::F32Ceil),
            &wasmparser::Operator::F32Floor => Ok(Operator::F32Floor),
            &wasmparser::Operator::F32Trunc => Ok(Operator::F32Trunc),
            &wasmparser::Operator::F32Nearest => Ok(Operator::F32Nearest),
            &wasmparser::Operator::F32Sqrt => Ok(Operator::F32Sqrt),
            &wasmparser::Operator::F32Add => Ok(Operator::F32Add),
            &wasmparser::Operator::F32Sub => Ok(Operator::F32Sub),
            &wasmparser::Operator::F32Mul => Ok(Operator::F32Mul),
            &wasmparser::Operator::F32Div => Ok(Operator::F32Div),
            &wasmparser::Operator::F32Min => Ok(Operator::F32Min),
            &wasmparser::Operator::F32Max => Ok(Operator::F32Max),
            &wasmparser::Operator::F32Copysign => Ok(Operator::F32Copysign),
            &wasmparser::Operator::F64Abs => Ok(Operator::F64Abs),
            &wasmparser::Operator::F64Neg => Ok(Operator::F64Neg),
            &wasmparser::Operator::F64Ceil => Ok(Operator::F64Ceil),
            &wasmparser::Operator::F64Floor => Ok(Operator::F64Floor),
            &wasmparser::Operator::F64Trunc => Ok(Operator::F64Trunc),
            &wasmparser::Operator::F64Nearest => Ok(Operator::F64Nearest),
            &wasmparser::Operator::F64Sqrt => Ok(Operator::F64Sqrt),
            &wasmparser::Operator::F64Add => Ok(Operator::F64Add),
            &wasmparser::Operator::F64Sub => Ok(Operator::F64Sub),
            &wasmparser::Operator::F64Mul => Ok(Operator::F64Mul),
            &wasmparser::Operator::F64Div => Ok(Operator::F64Div),
            &wasmparser::Operator::F64Min => Ok(Operator::F64Min),
            &wasmparser::Operator::F64Max => Ok(Operator::F64Max),
            &wasmparser::Operator::F64Copysign => Ok(Operator::F64Copysign),
            &wasmparser::Operator::I32WrapI64 => Ok(Operator::I32WrapI64),
            &wasmparser::Operator::I32TruncF32S => Ok(Operator::I32TruncF32S),
            &wasmparser::Operator::I32TruncF32U => Ok(Operator::I32TruncF32U),
            &wasmparser::Operator::I32TruncF64S => Ok(Operator::I32TruncF64S),
            &wasmparser::Operator::I32TruncF64U => Ok(Operator::I32TruncF64U),
            &wasmparser::Operator::I64ExtendI32S => Ok(Operator::I64ExtendI32S),
            &wasmparser::Operator::I64ExtendI32U => Ok(Operator::I64ExtendI32U),
            &wasmparser::Operator::I64TruncF32S => Ok(Operator::I64TruncF32S),
            &wasmparser::Operator::I64TruncF32U => Ok(Operator::I64TruncF32U),
            &wasmparser::Operator::I64TruncF64S => Ok(Operator::I64TruncF64S),
            &wasmparser::Operator::I64TruncF64U => Ok(Operator::I64TruncF64U),
            &wasmparser::Operator::F32ConvertI32S => Ok(Operator::F32ConvertI32S),
            &wasmparser::Operator::F32ConvertI32U => Ok(Operator::F32ConvertI32U),
            &wasmparser::Operator::F32ConvertI64S => Ok(Operator::F32ConvertI64S),
            &wasmparser::Operator::F32ConvertI64U => Ok(Operator::F32ConvertI64U),
            &wasmparser::Operator::F32DemoteF64 => Ok(Operator::F32DemoteF64),
            &wasmparser::Operator::F64ConvertI32S => Ok(Operator::F64ConvertI32S),
            &wasmparser::Operator::F64ConvertI32U => Ok(Operator::F64ConvertI32U),
            &wasmparser::Operator::F64ConvertI64S => Ok(Operator::F64ConvertI64S),
            &wasmparser::Operator::F64ConvertI64U => Ok(Operator::F64ConvertI64U),
            &wasmparser::Operator::F64PromoteF32 => Ok(Operator::F64PromoteF32),
            &wasmparser::Operator::I32Extend8S => Ok(Operator::I32Extend8S),
            &wasmparser::Operator::I32Extend16S => Ok(Operator::I32Extend16S),
            &wasmparser::Operator::I64Extend8S => Ok(Operator::I64Extend8S),
            &wasmparser::Operator::I64Extend16S => Ok(Operator::I64Extend16S),
            &wasmparser::Operator::I64Extend32S => Ok(Operator::I64Extend32S),
            &wasmparser::Operator::I32TruncSatF32S => Ok(Operator::I32TruncSatF32S),
            &wasmparser::Operator::I32TruncSatF32U => Ok(Operator::I32TruncSatF32U),
            &wasmparser::Operator::I32TruncSatF64S => Ok(Operator::I32TruncSatF64S),
            &wasmparser::Operator::I32TruncSatF64U => Ok(Operator::I32TruncSatF64U),
            &wasmparser::Operator::I64TruncSatF32S => Ok(Operator::I64TruncSatF32S),
            &wasmparser::Operator::I64TruncSatF32U => Ok(Operator::I64TruncSatF32U),
            &wasmparser::Operator::I64TruncSatF64S => Ok(Operator::I64TruncSatF64S),
            &wasmparser::Operator::I64TruncSatF64U => Ok(Operator::I64TruncSatF64U),
            &wasmparser::Operator::F32ReinterpretI32 => Ok(Operator::F32ReinterpretI32),
            &wasmparser::Operator::F64ReinterpretI64 => Ok(Operator::F64ReinterpretI64),
            &wasmparser::Operator::I32ReinterpretF32 => Ok(Operator::I32ReinterpretF32),
            &wasmparser::Operator::I64ReinterpretF64 => Ok(Operator::I64ReinterpretF64),
            &wasmparser::Operator::TableGet { table } => Ok(Operator::TableGet { table }),
            &wasmparser::Operator::TableSet { table } => Ok(Operator::TableSet { table }),
            &wasmparser::Operator::TableGrow { table } => Ok(Operator::TableGrow { table }),
            &wasmparser::Operator::TableSize { table } => Ok(Operator::TableSize { table }),
            &wasmparser::Operator::MemorySize { mem, .. } => Ok(Operator::MemorySize { mem }),
            &wasmparser::Operator::MemoryGrow { mem, .. } => Ok(Operator::MemoryGrow { mem }),
            _ => Err(()),
        }
    }
}

impl<'a> std::convert::Into<wasm_encoder::Instruction<'static>> for Operator {
    fn into(self) -> wasm_encoder::Instruction<'static> {
        match &self {
            &Operator::Unreachable => wasm_encoder::Instruction::Unreachable,
            &Operator::Nop => wasm_encoder::Instruction::Nop,
            &Operator::Call { function_index } => {
                wasm_encoder::Instruction::Call(function_index as u32)
            }
            &Operator::CallIndirect { index, table_index } => {
                wasm_encoder::Instruction::CallIndirect {
                    ty: index as u32,
                    table: table_index as u32,
                }
            }
            &Operator::Return => wasm_encoder::Instruction::Return,
            &Operator::LocalSet { local_index } => wasm_encoder::Instruction::LocalSet(local_index),
            &Operator::LocalTee { local_index } => wasm_encoder::Instruction::LocalTee(local_index),
            &Operator::LocalGet { local_index } => wasm_encoder::Instruction::LocalGet(local_index),
            &Operator::Select => wasm_encoder::Instruction::Select,
            &Operator::TypedSelect { ty } => {
                wasm_encoder::Instruction::TypedSelect(ty_to_valty(ty))
            }
            &Operator::GlobalGet { global_index } => {
                wasm_encoder::Instruction::GlobalGet(global_index)
            }
            &Operator::GlobalSet { global_index } => {
                wasm_encoder::Instruction::GlobalSet(global_index)
            }
            &Operator::I32Load { memory } => wasm_encoder::Instruction::I32Load(memory.into()),
            &Operator::I64Load { memory } => wasm_encoder::Instruction::I64Load(memory.into()),
            &Operator::F32Load { memory } => wasm_encoder::Instruction::F32Load(memory.into()),
            &Operator::F64Load { memory } => wasm_encoder::Instruction::F64Load(memory.into()),
            &Operator::I32Load8S { memory } => wasm_encoder::Instruction::I32Load8_S(memory.into()),
            &Operator::I32Load8U { memory } => wasm_encoder::Instruction::I32Load8_U(memory.into()),
            &Operator::I32Load16S { memory } => {
                wasm_encoder::Instruction::I32Load16_S(memory.into())
            }
            &Operator::I32Load16U { memory } => {
                wasm_encoder::Instruction::I32Load16_U(memory.into())
            }
            &Operator::I64Load8S { memory } => wasm_encoder::Instruction::I64Load8_S(memory.into()),
            &Operator::I64Load8U { memory } => wasm_encoder::Instruction::I64Load8_U(memory.into()),
            &Operator::I64Load16S { memory } => {
                wasm_encoder::Instruction::I64Load16_S(memory.into())
            }
            &Operator::I64Load16U { memory } => {
                wasm_encoder::Instruction::I64Load16_U(memory.into())
            }
            &Operator::I64Load32S { memory } => {
                wasm_encoder::Instruction::I64Load32_S(memory.into())
            }
            &Operator::I64Load32U { memory } => {
                wasm_encoder::Instruction::I64Load32_U(memory.into())
            }
            &Operator::I32Store { memory } => wasm_encoder::Instruction::I32Store(memory.into()),
            &Operator::I64Store { memory } => wasm_encoder::Instruction::I64Store(memory.into()),
            &Operator::F32Store { memory } => wasm_encoder::Instruction::F32Store(memory.into()),
            &Operator::F64Store { memory } => wasm_encoder::Instruction::F64Store(memory.into()),
            &Operator::I32Store8 { memory } => wasm_encoder::Instruction::I32Store8(memory.into()),
            &Operator::I32Store16 { memory } => {
                wasm_encoder::Instruction::I32Store16(memory.into())
            }
            &Operator::I64Store8 { memory } => wasm_encoder::Instruction::I64Store8(memory.into()),
            &Operator::I64Store16 { memory } => {
                wasm_encoder::Instruction::I64Store16(memory.into())
            }
            &Operator::I64Store32 { memory } => {
                wasm_encoder::Instruction::I64Store32(memory.into())
            }
            &Operator::I32Const { value } => wasm_encoder::Instruction::I32Const(value),
            &Operator::I64Const { value } => wasm_encoder::Instruction::I64Const(value),
            &Operator::F32Const { value } => {
                wasm_encoder::Instruction::F32Const(f32::from_bits(value.bits()))
            }
            &Operator::F64Const { value } => {
                wasm_encoder::Instruction::F64Const(f64::from_bits(value.bits()))
            }
            &Operator::I32Eqz => wasm_encoder::Instruction::I32Eqz,
            &Operator::I32Eq => wasm_encoder::Instruction::I32Eq,
            &Operator::I32Ne => wasm_encoder::Instruction::I32Neq,
            &Operator::I32LtS => wasm_encoder::Instruction::I32LtS,
            &Operator::I32LtU => wasm_encoder::Instruction::I32LtU,
            &Operator::I32GtS => wasm_encoder::Instruction::I32GtS,
            &Operator::I32GtU => wasm_encoder::Instruction::I32GtU,
            &Operator::I32LeS => wasm_encoder::Instruction::I32LeS,
            &Operator::I32LeU => wasm_encoder::Instruction::I32LeU,
            &Operator::I32GeS => wasm_encoder::Instruction::I32GeS,
            &Operator::I32GeU => wasm_encoder::Instruction::I32GeU,
            &Operator::I64Eqz => wasm_encoder::Instruction::I64Eqz,
            &Operator::I64Eq => wasm_encoder::Instruction::I64Eq,
            &Operator::I64Ne => wasm_encoder::Instruction::I64Neq,
            &Operator::I64LtS => wasm_encoder::Instruction::I64LtS,
            &Operator::I64LtU => wasm_encoder::Instruction::I64LtU,
            &Operator::I64GtU => wasm_encoder::Instruction::I64GtU,
            &Operator::I64GtS => wasm_encoder::Instruction::I64GtS,
            &Operator::I64LeS => wasm_encoder::Instruction::I64LeS,
            &Operator::I64LeU => wasm_encoder::Instruction::I64LeU,
            &Operator::I64GeS => wasm_encoder::Instruction::I64GeS,
            &Operator::I64GeU => wasm_encoder::Instruction::I64GeU,
            &Operator::F32Eq => wasm_encoder::Instruction::F32Eq,
            &Operator::F32Ne => wasm_encoder::Instruction::F32Neq,
            &Operator::F32Lt => wasm_encoder::Instruction::F32Lt,
            &Operator::F32Gt => wasm_encoder::Instruction::F32Gt,
            &Operator::F32Le => wasm_encoder::Instruction::F32Le,
            &Operator::F32Ge => wasm_encoder::Instruction::F32Ge,
            &Operator::F64Eq => wasm_encoder::Instruction::F64Eq,
            &Operator::F64Ne => wasm_encoder::Instruction::F64Neq,
            &Operator::F64Lt => wasm_encoder::Instruction::F64Lt,
            &Operator::F64Gt => wasm_encoder::Instruction::F64Gt,
            &Operator::F64Le => wasm_encoder::Instruction::F64Le,
            &Operator::F64Ge => wasm_encoder::Instruction::F64Ge,
            &Operator::I32Clz => wasm_encoder::Instruction::I32Clz,
            &Operator::I32Ctz => wasm_encoder::Instruction::I32Ctz,
            &Operator::I32Popcnt => wasm_encoder::Instruction::I32Popcnt,
            &Operator::I32Add => wasm_encoder::Instruction::I32Add,
            &Operator::I32Sub => wasm_encoder::Instruction::I32Sub,
            &Operator::I32Mul => wasm_encoder::Instruction::I32Mul,
            &Operator::I32DivS => wasm_encoder::Instruction::I32DivS,
            &Operator::I32DivU => wasm_encoder::Instruction::I32DivU,
            &Operator::I32RemS => wasm_encoder::Instruction::I32RemS,
            &Operator::I32RemU => wasm_encoder::Instruction::I32RemU,
            &Operator::I32And => wasm_encoder::Instruction::I32And,
            &Operator::I32Or => wasm_encoder::Instruction::I32Or,
            &Operator::I32Xor => wasm_encoder::Instruction::I32Xor,
            &Operator::I32Shl => wasm_encoder::Instruction::I32Shl,
            &Operator::I32ShrS => wasm_encoder::Instruction::I32ShrS,
            &Operator::I32ShrU => wasm_encoder::Instruction::I32ShrU,
            &Operator::I32Rotl => wasm_encoder::Instruction::I32Rotl,
            &Operator::I32Rotr => wasm_encoder::Instruction::I32Rotr,
            &Operator::I64Clz => wasm_encoder::Instruction::I64Clz,
            &Operator::I64Ctz => wasm_encoder::Instruction::I64Ctz,
            &Operator::I64Popcnt => wasm_encoder::Instruction::I64Popcnt,
            &Operator::I64Add => wasm_encoder::Instruction::I64Add,
            &Operator::I64Sub => wasm_encoder::Instruction::I64Sub,
            &Operator::I64Mul => wasm_encoder::Instruction::I64Mul,
            &Operator::I64DivS => wasm_encoder::Instruction::I64DivS,
            &Operator::I64DivU => wasm_encoder::Instruction::I64DivU,
            &Operator::I64RemS => wasm_encoder::Instruction::I64RemS,
            &Operator::I64RemU => wasm_encoder::Instruction::I64RemU,
            &Operator::I64And => wasm_encoder::Instruction::I64And,
            &Operator::I64Or => wasm_encoder::Instruction::I64Or,
            &Operator::I64Xor => wasm_encoder::Instruction::I64Xor,
            &Operator::I64Shl => wasm_encoder::Instruction::I64Shl,
            &Operator::I64ShrS => wasm_encoder::Instruction::I64ShrS,
            &Operator::I64ShrU => wasm_encoder::Instruction::I64ShrU,
            &Operator::I64Rotl => wasm_encoder::Instruction::I64Rotl,
            &Operator::I64Rotr => wasm_encoder::Instruction::I64Rotr,
            &Operator::F32Abs => wasm_encoder::Instruction::F32Abs,
            &Operator::F32Neg => wasm_encoder::Instruction::F32Neg,
            &Operator::F32Ceil => wasm_encoder::Instruction::F32Ceil,
            &Operator::F32Floor => wasm_encoder::Instruction::F32Floor,
            &Operator::F32Trunc => wasm_encoder::Instruction::F32Trunc,
            &Operator::F32Nearest => wasm_encoder::Instruction::F32Nearest,
            &Operator::F32Sqrt => wasm_encoder::Instruction::F32Sqrt,
            &Operator::F32Add => wasm_encoder::Instruction::F32Add,
            &Operator::F32Sub => wasm_encoder::Instruction::F32Sub,
            &Operator::F32Mul => wasm_encoder::Instruction::F32Mul,
            &Operator::F32Div => wasm_encoder::Instruction::F32Div,
            &Operator::F32Min => wasm_encoder::Instruction::F32Min,
            &Operator::F32Max => wasm_encoder::Instruction::F32Max,
            &Operator::F32Copysign => wasm_encoder::Instruction::F32Copysign,
            &Operator::F64Abs => wasm_encoder::Instruction::F64Abs,
            &Operator::F64Neg => wasm_encoder::Instruction::F64Neg,
            &Operator::F64Ceil => wasm_encoder::Instruction::F64Ceil,
            &Operator::F64Floor => wasm_encoder::Instruction::F64Floor,
            &Operator::F64Trunc => wasm_encoder::Instruction::F64Trunc,
            &Operator::F64Nearest => wasm_encoder::Instruction::F64Nearest,
            &Operator::F64Sqrt => wasm_encoder::Instruction::F64Sqrt,
            &Operator::F64Add => wasm_encoder::Instruction::F64Add,
            &Operator::F64Sub => wasm_encoder::Instruction::F64Sub,
            &Operator::F64Mul => wasm_encoder::Instruction::F64Mul,
            &Operator::F64Div => wasm_encoder::Instruction::F64Div,
            &Operator::F64Min => wasm_encoder::Instruction::F64Min,
            &Operator::F64Max => wasm_encoder::Instruction::F64Max,
            &Operator::F64Copysign => wasm_encoder::Instruction::F64Copysign,
            &Operator::I32WrapI64 => wasm_encoder::Instruction::I32WrapI64,
            &Operator::I32TruncF32S => wasm_encoder::Instruction::I32TruncF32S,
            &Operator::I32TruncF32U => wasm_encoder::Instruction::I32TruncF32U,
            &Operator::I32TruncF64S => wasm_encoder::Instruction::I32TruncF64S,
            &Operator::I32TruncF64U => wasm_encoder::Instruction::I32TruncF64U,
            &Operator::I64ExtendI32S => wasm_encoder::Instruction::I64ExtendI32S,
            &Operator::I64ExtendI32U => wasm_encoder::Instruction::I64ExtendI32U,
            &Operator::I64TruncF32S => wasm_encoder::Instruction::I64TruncF32S,
            &Operator::I64TruncF32U => wasm_encoder::Instruction::I64TruncF32U,
            &Operator::I64TruncF64S => wasm_encoder::Instruction::I64TruncF64S,
            &Operator::I64TruncF64U => wasm_encoder::Instruction::I64TruncF64U,
            &Operator::F32ConvertI32S => wasm_encoder::Instruction::F32ConvertI32S,
            &Operator::F32ConvertI32U => wasm_encoder::Instruction::F32ConvertI32U,
            &Operator::F32ConvertI64S => wasm_encoder::Instruction::F32ConvertI64S,
            &Operator::F32ConvertI64U => wasm_encoder::Instruction::F32ConvertI64U,
            &Operator::F32DemoteF64 => wasm_encoder::Instruction::F32DemoteF64,
            &Operator::F64ConvertI32S => wasm_encoder::Instruction::F64ConvertI32S,
            &Operator::F64ConvertI32U => wasm_encoder::Instruction::F64ConvertI32U,
            &Operator::F64ConvertI64S => wasm_encoder::Instruction::F64ConvertI64S,
            &Operator::F64ConvertI64U => wasm_encoder::Instruction::F64ConvertI64U,
            &Operator::F64PromoteF32 => wasm_encoder::Instruction::F64PromoteF32,
            &Operator::I32Extend8S => wasm_encoder::Instruction::I32Extend8S,
            &Operator::I32Extend16S => wasm_encoder::Instruction::I32Extend16S,
            &Operator::I64Extend8S => wasm_encoder::Instruction::I64Extend8S,
            &Operator::I64Extend16S => wasm_encoder::Instruction::I64Extend16S,
            &Operator::I64Extend32S => wasm_encoder::Instruction::I64Extend32S,
            &Operator::I32TruncSatF32S => wasm_encoder::Instruction::I32TruncSatF32S,
            &Operator::I32TruncSatF32U => wasm_encoder::Instruction::I32TruncSatF32U,
            &Operator::I32TruncSatF64S => wasm_encoder::Instruction::I32TruncSatF64S,
            &Operator::I32TruncSatF64U => wasm_encoder::Instruction::I32TruncSatF64U,
            &Operator::I64TruncSatF32S => wasm_encoder::Instruction::I64TruncSatF32S,
            &Operator::I64TruncSatF32U => wasm_encoder::Instruction::I64TruncSatF32U,
            &Operator::I64TruncSatF64S => wasm_encoder::Instruction::I64TruncSatF64S,
            &Operator::I64TruncSatF64U => wasm_encoder::Instruction::I64TruncSatF64U,
            &Operator::F32ReinterpretI32 => wasm_encoder::Instruction::F32ReinterpretI32,
            &Operator::F64ReinterpretI64 => wasm_encoder::Instruction::F64ReinterpretI64,
            &Operator::I32ReinterpretF32 => wasm_encoder::Instruction::I32ReinterpretF32,
            &Operator::I64ReinterpretF64 => wasm_encoder::Instruction::I64ReinterpretF64,
            &Operator::TableGet { table } => wasm_encoder::Instruction::TableGet { table },
            &Operator::TableSet { table } => wasm_encoder::Instruction::TableSet { table },
            &Operator::TableGrow { table } => wasm_encoder::Instruction::TableGrow { table },
            &Operator::TableSize { table } => wasm_encoder::Instruction::TableSize { table },
            &Operator::MemorySize { mem } => wasm_encoder::Instruction::MemorySize(mem),
            &Operator::MemoryGrow { mem } => wasm_encoder::Instruction::MemoryGrow(mem),
        }
    }
}

impl std::convert::From<MemoryImmediate> for Memory {
    fn from(value: MemoryImmediate) -> Memory {
        Memory {
            align: value.align,
            offset: value.offset,
            memory: value.memory as MemoryId,
        }
    }
}

impl std::convert::Into<wasm_encoder::MemArg> for Memory {
    fn into(self) -> wasm_encoder::MemArg {
        wasm_encoder::MemArg {
            align: self.align as u32,
            offset: self.offset as u64,
            memory_index: self.memory as u32,
        }
    }
}

pub fn ty_to_valty(ty: Type) -> wasm_encoder::ValType {
    match ty {
        Type::I32 => wasm_encoder::ValType::I32,
        Type::I64 => wasm_encoder::ValType::I64,
        Type::F32 => wasm_encoder::ValType::F32,
        Type::F64 => wasm_encoder::ValType::F64,
        _ => panic!("Unsupported type: {:?}", ty),
    }
}
