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
    TypedFuncRef(bool, u32),
}
impl From<wasmparser::ValType> for Type {
    fn from(ty: wasmparser::ValType) -> Self {
        match ty {
            wasmparser::ValType::I32 => Type::I32,
            wasmparser::ValType::I64 => Type::I64,
            wasmparser::ValType::F32 => Type::F32,
            wasmparser::ValType::F64 => Type::F64,
            wasmparser::ValType::V128 => Type::V128,
            wasmparser::ValType::Ref(r) => r.into(),
        }
    }
}
impl From<wasmparser::RefType> for Type {
    fn from(ty: wasmparser::RefType) -> Self {
        match ty.type_index() {
            Some(idx) => {
                let nullable = ty.is_nullable();
                Type::TypedFuncRef(nullable, idx.as_module_index().unwrap())
            }
            None => Type::FuncRef,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::V128 => write!(f, "v128"),
            Type::FuncRef => write!(f, "funcref"),
            Type::TypedFuncRef(nullable, idx) => write!(
                f,
                "funcref({}, {})",
                if *nullable { "null" } else { "not_null" },
                idx
            ),
        }
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
            Type::FuncRef | Type::TypedFuncRef(..) => wasm_encoder::ValType::Ref(ty.into()),
        }
    }
}

impl From<Type> for wasm_encoder::RefType {
    fn from(ty: Type) -> wasm_encoder::RefType {
        match ty {
            Type::FuncRef => wasm_encoder::RefType::FUNCREF,
            Type::TypedFuncRef(nullable, idx) => wasm_encoder::RefType {
                nullable,
                heap_type: wasm_encoder::HeapType::Concrete(idx),
            },
            _ => panic!("Cannot convert {:?} into reftype", ty),
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
mod debug;
pub use debug::*;
