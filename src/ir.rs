//! Intermediate representation for Wasm.

use crate::{backend::Shape, cfg::CFGInfo, frontend};
use anyhow::Result;
use fxhash::FxHashMap;
use wasmparser::{FuncType, Operator, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;
pub type LocalId = u32;

pub const INVALID_BLOCK: BlockId = usize::MAX;

#[derive(Clone, Debug, Default)]
pub struct Module<'a> {
    pub orig_bytes: &'a [u8],
    pub funcs: Vec<FuncDecl>,
    pub signatures: Vec<FuncType>,
    pub globals: Vec<Type>,
    pub tables: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum FuncDecl {
    Import(SignatureId),
    Body(SignatureId, FunctionBody),
}

impl FuncDecl {
    pub fn sig(&self) -> SignatureId {
        match self {
            FuncDecl::Import(sig) => *sig,
            FuncDecl::Body(sig, ..) => *sig,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct FunctionBody {
    pub arg_values: Vec<Value>,
    pub locals: Vec<Type>,
    pub blocks: Vec<Block>,
    pub types: FxHashMap<Value, Type>,
}

#[derive(Clone, Debug, Default)]
pub struct Block {
    pub id: BlockId,
    pub params: Vec<Type>,
    pub insts: Vec<Inst>,
    pub terminator: Terminator,
}

impl Block {
    pub fn successors(&self) -> Vec<BlockId> {
        self.terminator.successors()
    }

    pub fn defs<'b>(&'b self) -> impl Iterator<Item = Value> + 'b {
        let block = self.id;
        let param_values = (0..self.params.len()).map(move |i| Value::blockparam(block, i));
        let inst_values = self
            .insts
            .iter()
            .enumerate()
            .map(move |(inst_id, inst)| {
                (0..inst.n_outputs).map(move |i| Value::inst(block, inst_id, i))
            })
            .flatten();
        param_values.chain(inst_values)
    }

    pub fn visit_uses<F: FnMut(Value)>(&self, mut f: F) {
        for inst in &self.insts {
            for &input in &inst.inputs {
                f(input);
            }
        }
        self.terminator.visit_uses(f);
    }

    pub fn update_uses<F: FnMut(&mut Value)>(&mut self, mut f: F) {
        for inst in &mut self.insts {
            for input in &mut inst.inputs {
                f(input);
            }
        }
        self.terminator.update_uses(f);
    }
}

#[derive(Clone, Debug)]
pub struct Inst {
    pub operator: Operator<'static>,
    pub n_outputs: usize,
    pub inputs: Vec<Value>,
}

impl Inst {
    pub fn make<'a>(operator: &Operator<'a>, n_outputs: usize, inputs: Vec<Value>) -> Self {
        // The only operator that actually makes use of the lifetime
        // parameter is BrTable.
        assert!(!matches!(operator, &Operator::BrTable { .. }));
        let operator = operator.clone();
        let operator = unsafe { std::mem::transmute(operator) };
        Inst {
            operator,
            n_outputs,
            inputs,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u64)]
enum ValueTag {
    /// Undefined value. Fields: tag(4) unused(60).
    Undef = 0,
    /// Function argument. Fields: tag(4) unused(52) index(8).
    Arg = 1,
    /// Block param. Fields: tag(4) unused(2) block(26) param(32).
    BlockParam = 2,
    /// Instruction output. Fields: tag(4) block(26) inst(26) output(8).
    InstOutput = 3,
}

const VALUE_TAG_SHIFT: usize = 60;

impl Value {
    pub fn undef() -> Self {
        Value((ValueTag::Undef as u64) << VALUE_TAG_SHIFT)
    }

    pub fn arg(index: usize) -> Self {
        assert!(index < 256);
        Value(((ValueTag::Arg as u64) << VALUE_TAG_SHIFT) | (index as u64))
    }

    pub fn blockparam(block: BlockId, index: usize) -> Self {
        assert!(index < 256);
        assert!(block < (1 << 26));
        Value(
            ((ValueTag::BlockParam as u64) << VALUE_TAG_SHIFT)
                | ((block as u64) << 32)
                | (index as u64),
        )
    }

    pub fn inst(block: BlockId, inst: InstId, index: usize) -> Self {
        assert!(index < 256);
        assert!(block < (1 << 26));
        assert!(inst < (1 << 26));
        Value(
            ((ValueTag::InstOutput as u64) << VALUE_TAG_SHIFT)
                | ((block as u64) << 34)
                | ((inst as u64) << 8)
                | (index as u64),
        )
    }

    pub fn unpack(self) -> ValueKind {
        let tag = self.0 >> VALUE_TAG_SHIFT;
        match tag {
            0 => ValueKind::Undef,
            1 => ValueKind::Arg((self.0 & ((1 << 8) - 1)) as usize),
            2 => ValueKind::BlockParam(
                ((self.0 >> 32) & ((1 << 26) - 1)) as usize,
                (self.0 & 0xff) as usize,
            ),
            3 => ValueKind::Inst(
                ((self.0 >> 34) & ((1 << 26) - 1)) as usize,
                ((self.0 >> 8) & ((1 << 26) - 1)) as usize,
                (self.0 & 0xff) as usize,
            ),
            _ => unreachable!(),
        }
    }

    pub fn as_arg(self) -> Option<usize> {
        match self.unpack() {
            ValueKind::Arg(arg) => Some(arg),
            _ => None,
        }
    }

    pub fn as_blockparam(self) -> Option<(BlockId, usize)> {
        match self.unpack() {
            ValueKind::BlockParam(block, param) => Some((block, param)),
            _ => None,
        }
    }

    pub fn as_inst(self) -> Option<(BlockId, InstId, usize)> {
        match self.unpack() {
            ValueKind::Inst(block, inst, param) => Some((block, inst, param)),
            _ => None,
        }
    }

    pub fn index(self) -> u64 {
        self.0
    }

    pub fn from_index(value: u64) -> Value {
        let tag = value >> VALUE_TAG_SHIFT;
        assert!(tag < 4);
        Self(value)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.unpack() {
            ValueKind::Undef => write!(f, "undef"),
            ValueKind::Arg(i) => write!(f, "arg{}", i),
            ValueKind::BlockParam(block, i) => write!(f, "block{}_{}", block, i),
            ValueKind::Inst(block, inst, i) => write!(f, "inst{}_{}_{}", block, inst, i),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueKind {
    Undef,
    Arg(usize),
    BlockParam(BlockId, usize),
    Inst(BlockId, InstId, usize),
}

#[derive(Clone, Debug)]
pub struct BlockTarget {
    pub block: BlockId,
    pub args: Vec<Value>,
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Br {
        target: BlockTarget,
    },
    CondBr {
        cond: Value,
        if_true: BlockTarget,
        if_false: BlockTarget,
    },
    Select {
        value: Value,
        targets: Vec<BlockTarget>,
        default: BlockTarget,
    },
    Return {
        values: Vec<Value>,
    },
    None,
}

impl std::default::Default for Terminator {
    fn default() -> Self {
        Terminator::None
    }
}

impl Terminator {
    pub fn args(&self) -> Vec<Value> {
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

    pub fn visit_uses<F: FnMut(Value)>(&self, mut f: F) {
        match self {
            &Terminator::CondBr { cond, .. } => f(cond),
            &Terminator::Select { value, .. } => f(value),
            &Terminator::Return { ref values, .. } => {
                for &value in values {
                    f(value);
                }
            }
            _ => {}
        }
    }

    pub fn update_uses<F: FnMut(&mut Value)>(&mut self, mut f: F) {
        match self {
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
