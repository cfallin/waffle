//! Intermediate representation for Wasm.

use crate::declare_entity;

/// Types in waffle's IR.
///
/// These types correspond to (a subset of) the primitive Wasm value
/// types: integers, floats, SIMD vectors, and function references
/// (optionally typed).
///
/// Every SSA value in a function body has a `Type`, unless it is a
/// tuple (multi-value or zero-value result).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    /// A 32-bit integer. Signedness is unspecified: individual
    /// operators specify how they handle sign.
    I32,
    /// A 64-bit integer. Signedness is unspecified: individual
    /// operators specify how they handle sign.
    I64,
    /// A 32-bit IEEE 754 floating point value. Semantics around NaNs
    /// are defined by individual operators; from the point of view of
    /// IR scaffolding, floating-point values are bags of bits.
    F32,
    /// A 64-bit IEEE 754 floating point value. Semantics around NaNs
    /// are defined by individual operators; from the point of view of
    /// IR scaffolding, floating-point values are bags of bits.
    F64,
    /// A 128-bit SIMD vector value. Lanes and lane types are
    /// specified by individual operators; from the point of view of
    /// IR scaffolding, SIMD vector values are bags of bits.
    V128,
    /// A function reference.
    FuncRef,
    /// A typed function reference, optionally nullable, and with type
    /// specified by a signature index in the module's signature
    /// index-space.
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

// Per-module index spaces:

// A signature (list of parameter types and list of return types) in
// the module.
declare_entity!(Signature, "sig");
// A function in the module.
declare_entity!(Func, "func");
// A global variable in the module.
declare_entity!(Global, "global");
// A table in the module.
declare_entity!(Table, "table");
// A memory in the module.
declare_entity!(Memory, "memory");

// Per-function index spaces:

// A basic block in one function body.
declare_entity!(Block, "block");
// A local variable (storage slot) in one function body.
declare_entity!(Local, "local");
// An SSA value in one function body.
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
