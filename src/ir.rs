//! Intermediate representation for Wasm.

use wasmparser::{FuncType, Type};

pub type SignatureId = usize;
pub type FuncId = usize;

#[derive(Clone, Debug)]
pub struct Module {
    pub funcs: Vec<FuncDecl>,
    pub signatures: Vec<FuncType>,
}

#[derive(Clone, Debug)]
pub enum FuncDecl {
    Import(SignatureId),
    Body(SignatureId, Function),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: FuncId,
    pub signature: SignatureId,
    pub locals: Vec<Type>,
}
