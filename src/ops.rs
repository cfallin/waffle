//! Operators.

use wasmparser::{Ieee32, Ieee64, MemoryImmediate};

use crate::{Func, Global, Local, Memory, Signature, Table, Type};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MemoryArg {
    pub align: u8,
    pub offset: u64,
    pub memory: Memory,
}

impl std::fmt::Display for MemoryArg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}, align={}, offset={}",
            self.memory, self.align, self.offset
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Operator {
    Unreachable,
    Nop,

    Call {
        function_index: Func,
    },
    CallIndirect {
        sig_index: Signature,
        table_index: Table,
    },
    Return,
    LocalSet {
        local_index: Local,
    },
    LocalTee {
        local_index: Local,
    },
    LocalGet {
        local_index: Local,
    },
    Select,
    TypedSelect {
        ty: Type,
    },
    GlobalGet {
        global_index: Global,
    },
    GlobalSet {
        global_index: Global,
    },

    I32Load {
        memory: MemoryArg,
    },
    I64Load {
        memory: MemoryArg,
    },
    F32Load {
        memory: MemoryArg,
    },
    F64Load {
        memory: MemoryArg,
    },
    I32Load8S {
        memory: MemoryArg,
    },
    I32Load8U {
        memory: MemoryArg,
    },
    I32Load16S {
        memory: MemoryArg,
    },
    I32Load16U {
        memory: MemoryArg,
    },
    I64Load8S {
        memory: MemoryArg,
    },
    I64Load8U {
        memory: MemoryArg,
    },
    I64Load16S {
        memory: MemoryArg,
    },
    I64Load16U {
        memory: MemoryArg,
    },
    I64Load32S {
        memory: MemoryArg,
    },
    I64Load32U {
        memory: MemoryArg,
    },

    I32Store {
        memory: MemoryArg,
    },
    I64Store {
        memory: MemoryArg,
    },
    F32Store {
        memory: MemoryArg,
    },
    F64Store {
        memory: MemoryArg,
    },
    I32Store8 {
        memory: MemoryArg,
    },
    I32Store16 {
        memory: MemoryArg,
    },
    I64Store8 {
        memory: MemoryArg,
    },
    I64Store16 {
        memory: MemoryArg,
    },
    I64Store32 {
        memory: MemoryArg,
    },

    I32Const {
        value: i32,
    },
    I64Const {
        value: i64,
    },
    F32Const {
        value: Ieee32,
    },
    F64Const {
        value: Ieee64,
    },

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
    TableGet {
        table_index: Table,
    },
    TableSet {
        table_index: Table,
    },
    TableGrow {
        table_index: Table,
    },
    TableSize {
        table_index: Table,
    },
    MemorySize {
        mem: Memory,
    },
    MemoryGrow {
        mem: Memory,
    },
}

impl<'a, 'b> std::convert::TryFrom<&'b wasmparser::Operator<'a>> for Operator {
    type Error = ();

    fn try_from(op: &'b wasmparser::Operator<'a>) -> Result<Operator, Self::Error> {
        match op {
            &wasmparser::Operator::Unreachable => Ok(Operator::Unreachable),
            &wasmparser::Operator::Nop => Ok(Operator::Nop),
            &wasmparser::Operator::Call { function_index } => Ok(Operator::Call {
                function_index: Func::from(function_index),
            }),
            &wasmparser::Operator::CallIndirect {
                index, table_index, ..
            } => Ok(Operator::CallIndirect {
                sig_index: Signature::from(index),
                table_index: Table::from(table_index),
            }),
            &wasmparser::Operator::Return => Ok(Operator::Return),
            &wasmparser::Operator::LocalSet { local_index } => Ok(Operator::LocalSet {
                local_index: Local::from(local_index),
            }),
            &wasmparser::Operator::LocalTee { local_index } => Ok(Operator::LocalTee {
                local_index: Local::from(local_index),
            }),
            &wasmparser::Operator::LocalGet { local_index } => Ok(Operator::LocalGet {
                local_index: Local::from(local_index),
            }),
            &wasmparser::Operator::Select => Ok(Operator::Select),
            &wasmparser::Operator::TypedSelect { ty } => Ok(Operator::TypedSelect { ty: ty.into() }),
            &wasmparser::Operator::GlobalGet { global_index } => Ok(Operator::GlobalGet {
                global_index: Global::from(global_index),
            }),
            &wasmparser::Operator::GlobalSet { global_index } => Ok(Operator::GlobalSet {
                global_index: Global::from(global_index),
            }),
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
            &wasmparser::Operator::TableGet { table } => Ok(Operator::TableGet {
                table_index: Table::from(table),
            }),
            &wasmparser::Operator::TableSet { table } => Ok(Operator::TableSet {
                table_index: Table::from(table),
            }),
            &wasmparser::Operator::TableGrow { table } => Ok(Operator::TableGrow {
                table_index: Table::from(table),
            }),
            &wasmparser::Operator::TableSize { table } => Ok(Operator::TableSize {
                table_index: Table::from(table),
            }),
            &wasmparser::Operator::MemorySize { mem, .. } => Ok(Operator::MemorySize {
                mem: Memory::from(mem),
            }),
            &wasmparser::Operator::MemoryGrow { mem, .. } => Ok(Operator::MemoryGrow {
                mem: Memory::from(mem),
            }),
            _ => Err(()),
        }
    }
}

impl std::convert::From<MemoryImmediate> for MemoryArg {
    fn from(value: MemoryImmediate) -> MemoryArg {
        MemoryArg {
            align: value.align,
            offset: value.offset,
            memory: Memory::from(value.memory),
        }
    }
}
