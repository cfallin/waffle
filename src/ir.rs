//! Intermediate representation for Wasm.

use crate::entity;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    V128,
    FuncRef,
}
impl From<wasmparser::Type> for Type {
    fn from(ty: wasmparser::Type) -> Self {
        match ty {
            wasmparser::Type::I32 => Type::I32,
            wasmparser::Type::I64 => Type::I64,
            wasmparser::Type::F32 => Type::F32,
            wasmparser::Type::F64 => Type::F64,
            wasmparser::Type::V128 => Type::V128,
            wasmparser::Type::FuncRef => Type::FuncRef,
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

entity!(Signature, "sig");
entity!(Func, "func");
entity!(Block, "block");
entity!(Local, "local");
entity!(Global, "global");
entity!(Table, "table");
entity!(Memory, "memory");
entity!(Value, "v");

mod module;
pub use module::*;
mod func;
pub use func::*;
mod value;
pub use value::*;
mod display;
pub use display::*;
