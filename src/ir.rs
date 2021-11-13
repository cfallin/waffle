//! Intermediate representation for Wasm.

use wasmparser::{FuncType, Operator, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;
pub type ValueId = usize;

#[derive(Clone, Debug, Default)]
pub struct Module<'a> {
    pub funcs: Vec<FuncDecl<'a>>,
    pub signatures: Vec<FuncType>,
}

#[derive(Clone, Debug)]
pub enum FuncDecl<'a> {
    Import(SignatureId),
    Body(SignatureId, FunctionBody<'a>),
}

impl<'a> FuncDecl<'a> {
    pub fn sig(&self) -> SignatureId {
        match self {
            FuncDecl::Import(sig) => *sig,
            FuncDecl::Body(sig, ..) => *sig,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct FunctionBody<'a> {
    pub locals: Vec<Type>,
    pub blocks: Vec<Block<'a>>,
    pub values: Vec<ValueDef>,
}

#[derive(Clone, Debug)]
pub struct ValueDef {
    pub kind: ValueKind,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum ValueKind {
    BlockParam(BlockId, usize),
    Inst(BlockId, InstId),
}

#[derive(Clone, Debug, Default)]
pub struct Block<'a> {
    pub params: Vec<Type>,
    pub insts: Vec<Inst<'a>>,
    pub terminator: Terminator<'a>,
}

#[derive(Clone, Debug)]
pub struct Inst<'a> {
    pub operator: Operator<'a>,
    pub outputs: Vec<ValueId>,
    pub inputs: Vec<Operand<'a>>,
}

#[derive(Clone, Debug)]
pub enum Operand<'a> {
    Value(ValueId),
    Sub(Box<Inst<'a>>),
}

#[derive(Clone, Debug)]
pub enum Terminator<'a> {
    Br {
        target: BlockId,
    },
    CondBr {
        cond: Operand<'a>,
        if_true: BlockId,
        if_false: BlockId,
    },
    Select {
        value: Operand<'a>,
        targets: Vec<BlockId>,
        default: BlockId,
    },
    Return {
        values: Vec<Operand<'a>>,
    },
    None,
}

impl<'a> std::default::Default for Terminator<'a> {
    fn default() -> Self {
        Terminator::None
    }
}
