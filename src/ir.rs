//! Intermediate representation for Wasm.

use wasmparser::{FuncType, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;

#[derive(Clone, Debug)]
pub struct Module {
    pub funcs: Vec<FuncDecl>,
    pub signatures: Vec<FuncType>,
}

#[derive(Clone, Debug)]
pub enum FuncDecl {
    Import(SignatureId),
    Body(SignatureId, FunctionBody),
}

#[derive(Clone, Debug)]
pub struct FunctionBody {
    pub locals: Vec<Type>,
    pub blocks: Vec<Block>,
}
