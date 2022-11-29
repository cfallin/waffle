//! Intermediate representation for Wasm.

use crate::declare_entity;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    V128,
    FuncRef,
}
impl From<wasmparser::ValType> for Type {
    fn from(ty: wasmparser::ValType) -> Self {
        match ty {
            wasmparser::ValType::I32 => Type::I32,
            wasmparser::ValType::I64 => Type::I64,
            wasmparser::ValType::F32 => Type::F32,
            wasmparser::ValType::F64 => Type::F64,
            wasmparser::ValType::V128 => Type::V128,
            wasmparser::ValType::FuncRef => Type::FuncRef,
            _ => panic!("Unsupported type: {:?}", ty),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::V128 => "v128",
            Type::FuncRef => "funcref",
        };
        write!(f, "{}", s)
    }
}

impl From<Type> for wasm_encoder::ValType {
    fn from(ty: Type) -> wasm_encoder::ValType {
        match ty {
            Type::I32 => wasm_encoder::ValType::I32,
            Type::I64 => wasm_encoder::ValType::I64,
            Type::F32 => wasm_encoder::ValType::F32,
            Type::F64 => wasm_encoder::ValType::F64,
            Type::V128 => wasm_encoder::ValType::V128,
            Type::FuncRef => wasm_encoder::ValType::FuncRef,
        }
    }
}

declare_entity!(Signature, "sig");
declare_entity!(Func, "func");
declare_entity!(Block, "block");
declare_entity!(Local, "local");
declare_entity!(Global, "global");
declare_entity!(Table, "table");
declare_entity!(Memory, "memory");
declare_entity!(Value, "v");

mod module;
pub use module::*;
mod func;
pub use func::*;
mod value;
pub use value::*;
mod display;
pub use display::*;
