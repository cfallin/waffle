//! Intermediate representation for Wasm.

use wasmparser::{FuncType, Operator, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;
pub type ValueId = usize;

#[derive(Clone, Debug, Default)]
pub struct Module {
    pub funcs: Vec<FuncDecl>,
    pub signatures: Vec<FuncType>,
}

#[derive(Clone, Debug)]
pub enum FuncDecl {
    Import(SignatureId),
    Body(SignatureId, FunctionBody),
}

#[derive(Clone, Debug, Default)]
pub struct FunctionBody {
    pub locals: Vec<Type>,
    pub blocks: Vec<Block>,
    pub values: Vec<ValueDef>,
}

#[derive(Clone, Debug)]
pub struct ValueDef {
    pub kind: ValueKind,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum ValueKind {
    BlockParam(Block),
    Inst(Block, Inst),
}

#[derive(Clone, Debug, Default)]
pub struct Block {
    pub params: Vec<Type>,
    pub insts: Vec<Inst>,
}

#[derive(Clone, Debug)]
pub struct Inst {
    pub operator: Operator<'static>,
    pub outputs: Vec<ValueId>,
    pub inputs: Vec<Operand>,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Value(ValueId),
    Sub(Box<Inst>),
}
