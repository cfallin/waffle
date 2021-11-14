//! Intermediate representation for Wasm.

use wasmparser::{FuncType, Operator, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;
pub type ValueId = usize;

pub const NO_VALUE: ValueId = usize::MAX;

#[derive(Clone, Debug, Default)]
pub struct Module<'a> {
    pub funcs: Vec<FuncDecl<'a>>,
    pub signatures: Vec<FuncType>,
    pub globals: Vec<Type>,
    pub tables: Vec<Type>,
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
    /// An SSA value.
    Value(ValueId),
    /// Tree-ified instructions for Wasm emission.
    Sub(Box<Inst<'a>>),
    /// Undef values are produced when code is unreachable and thus
    /// removed/never executed.
    Undef,
}

impl<'a> Operand<'a> {
    pub fn value(value: ValueId) -> Self {
        if value == NO_VALUE {
            Operand::Undef
        } else {
            Operand::Value(value)
        }
    }
}

#[derive(Clone, Debug)]
pub struct BlockTarget<'a> {
    pub block: BlockId,
    pub args: Vec<Operand<'a>>,
}

#[derive(Clone, Debug)]
pub enum Terminator<'a> {
    Br {
        target: BlockTarget<'a>,
    },
    CondBr {
        cond: Operand<'a>,
        if_true: BlockTarget<'a>,
        if_false: BlockTarget<'a>,
    },
    Select {
        value: Operand<'a>,
        targets: Vec<BlockTarget<'a>>,
        default: BlockTarget<'a>,
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
