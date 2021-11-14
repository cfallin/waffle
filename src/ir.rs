//! Intermediate representation for Wasm.

use crate::frontend;
use anyhow::Result;
use wasmparser::{FuncType, Operator, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;
pub type ValueId = usize;
pub type LocalId = u32;

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
    pub local: Option<LocalId>,
}

#[derive(Clone, Debug)]
pub enum ValueKind {
    Arg(usize),
    BlockParam(BlockId, usize),
    Inst(BlockId, InstId, usize),
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

impl<'a> Module<'a> {
    pub fn from_wasm_bytes(bytes: &'a [u8]) -> Result<Self> {
        frontend::wasm_to_ir(bytes)
    }
}

impl<'a> Terminator<'a> {
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Terminator::Return { .. } => vec![],
            Terminator::Br { target, .. } => vec![target.block],
            Terminator::CondBr {
                if_true, if_false, ..
            } => vec![if_true.block, if_false.block],
            Terminator::Select {
                ref targets,
                default,
                ..
            } => {
                let mut ret = targets
                    .iter()
                    .map(|target| target.block)
                    .collect::<Vec<_>>();
                ret.push(default.block);
                ret
            }
            Terminator::None => vec![],
        }
    }
}
