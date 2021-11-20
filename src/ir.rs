//! Intermediate representation for Wasm.

use crate::{backend::Shape, cfg::CFGInfo, frontend};
use anyhow::Result;
use wasmparser::{FuncType, Operator, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;
pub type ValueId = usize;
pub type LocalId = u32;

pub const NO_VALUE: ValueId = usize::MAX;
pub const INVALID_BLOCK: BlockId = usize::MAX;

#[derive(Clone, Debug, Default)]
pub struct Module<'a> {
    pub orig_bytes: &'a [u8],
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
    Arg(usize),
    BlockParam(BlockId, usize),
    Inst(BlockId, InstId, usize),
}

#[derive(Clone, Debug, Default)]
pub struct Block<'a> {
    pub params: Vec<Type>,
    pub insts: Vec<Inst<'a>>,
    pub terminator: Terminator,
}

impl<'a> Block<'a> {
    pub fn successors(&self) -> Vec<BlockId> {
        self.terminator.successors()
    }

    pub fn values<'b>(&'b self) -> impl Iterator<Item = ValueId> + 'b {
        self.insts
            .iter()
            .map(|inst| inst.outputs.iter().cloned())
            .flatten()
    }

    pub fn visit_operands<F: Fn(&Operand)>(&self, f: F) {
        for inst in &self.insts {
            for input in &inst.inputs {
                f(input);
            }
        }
        match &self.terminator {
            &Terminator::CondBr { ref cond, .. } => f(cond),
            &Terminator::Select { ref value, .. } => f(value),
            &Terminator::Return { ref values, .. } => {
                for value in values {
                    f(value);
                }
            }
            _ => {}
        }
    }

    pub fn update_operands<F: Fn(&mut Operand)>(&mut self, f: F) {
        for inst in &mut self.insts {
            for input in &mut inst.inputs {
                f(input);
            }
        }
        match &mut self.terminator {
            &mut Terminator::CondBr { ref mut cond, .. } => f(cond),
            &mut Terminator::Select { ref mut value, .. } => f(value),
            &mut Terminator::Return { ref mut values, .. } => {
                for value in values {
                    f(value);
                }
            }
            _ => {}
        }
    }
}

#[derive(Clone, Debug)]
pub struct Inst<'a> {
    pub operator: Operator<'a>,
    pub outputs: Vec<ValueId>,
    pub inputs: Vec<Operand>,
}

#[derive(Clone, Copy, Debug)]
pub enum Operand {
    /// An SSA value.
    Value(ValueId),
    /// Undef values are produced when code is unreachable and thus
    /// removed/never executed.
    Undef,
}

impl Operand {
    pub fn value(value: ValueId) -> Self {
        if value == NO_VALUE {
            Operand::Undef
        } else {
            Operand::Value(value)
        }
    }
}

#[derive(Clone, Debug)]
pub struct BlockTarget {
    pub block: BlockId,
    pub args: Vec<Operand>,
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Br {
        target: BlockTarget,
    },
    CondBr {
        cond: Operand,
        if_true: BlockTarget,
        if_false: BlockTarget,
    },
    Select {
        value: Operand,
        targets: Vec<BlockTarget>,
        default: BlockTarget,
    },
    Return {
        values: Vec<Operand>,
    },
    None,
}

impl std::default::Default for Terminator {
    fn default() -> Self {
        Terminator::None
    }
}

impl Terminator {
    pub fn args(&self) -> Vec<Operand> {
        match self {
            Terminator::Br { target } => target.args.clone(),
            Terminator::CondBr {
                cond,
                if_true,
                if_false,
            } => {
                let mut ret = vec![*cond];
                ret.extend(if_true.args.iter().cloned());
                ret.extend(if_false.args.iter().cloned());
                ret
            }
            Terminator::Select {
                value,
                targets,
                default,
            } => {
                let mut ret = vec![*value];
                for target in targets {
                    ret.extend(target.args.iter().cloned());
                }
                ret.extend(default.args.clone());
                ret
            }
            Terminator::Return { values } => values.clone(),
            Terminator::None => vec![],
        }
    }
}

impl<'a> Module<'a> {
    pub fn from_wasm_bytes(bytes: &'a [u8]) -> Result<Self> {
        frontend::wasm_to_ir(bytes)
    }

    pub fn to_wasm_bytes(self) -> Vec<u8> {
        for func in &self.funcs {
            match func {
                &FuncDecl::Body(_, ref body) => {
                    let cfg = CFGInfo::new(body);
                    let _shape = Shape::compute(body, &cfg);
                }
                _ => {}
            }
        }
        // TODO
        self.orig_bytes.to_vec()
    }
}

impl Terminator {
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
