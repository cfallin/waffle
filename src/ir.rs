//! Intermediate representation for Wasm.

use crate::{backend::Shape, cfg::CFGInfo, frontend, Operator};
use anyhow::Result;
use fxhash::FxHashMap;
use wasmparser::{FuncType, Type};

pub type SignatureId = usize;
pub type FuncId = usize;
pub type BlockId = usize;
pub type InstId = usize;
pub type LocalId = u32;
pub type GlobalId = u32;
pub type TableId = u32;
pub type MemoryId = u32;

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
    pub locals: Vec<Type>,
    pub blocks: Vec<Block>,
    /// Sea-of-nodes representation.
    pub values: Vec<ValueDef>,
    value_dedup: FxHashMap<ValueDef, Value>,
    pub types: Vec</* Value, */ Option<Type>>,
}

impl FunctionBody {
    pub fn add_block(&mut self) -> BlockId {
        let id = self.blocks.len();
        self.blocks.push(Block::default());
        self.blocks[id].id = id;
        id
    }

    pub fn add_edge(&mut self, from: BlockId, to: BlockId) {
        let succ_pos = self.blocks[from].succs.len();
        let pred_pos = self.blocks[to].preds.len();
        self.blocks[from].succs.push(to);
        self.blocks[to].preds.push(from);
        self.blocks[from].pos_in_succ_pred.push(pred_pos);
        self.blocks[to].pos_in_pred_succ.push(succ_pos);
    }

    pub fn add_value(&mut self, value: ValueDef, ty: Option<Type>) -> Value {
        let id = Value(self.values.len() as u32);
        self.values.push(value);
        self.types.push(ty);
        id
    }

    pub fn append_to_block(&mut self, block: BlockId, value: Value) {
        self.blocks[block].insts.push(value);
    }

    pub fn end_block(&mut self, block: BlockId, terminator: Terminator) {
        terminator.visit_successors(|succ| {
            self.add_edge(block, succ);
        });
        self.blocks[block].terminator = terminator;
    }

    pub fn add_local(&mut self, ty: Type) -> LocalId {
        let id = self.locals.len() as LocalId;
        self.locals.push(ty);
        id
    }
}

impl std::ops::Index<Value> for FunctionBody {
    type Output = ValueDef;
    fn index(&self, index: Value) -> &ValueDef {
        &self.values[index.0 as usize]
    }
}
impl std::ops::IndexMut<Value> for FunctionBody {
    fn index_mut(&mut self, index: Value) -> &mut ValueDef {
        &mut self.values[index.0 as usize]
    }
}
impl std::ops::Index<BlockId> for FunctionBody {
    type Output = Block;
    fn index(&self, index: BlockId) -> &Block {
        &self.blocks[index]
    }
}
impl std::ops::IndexMut<BlockId> for FunctionBody {
    fn index_mut(&mut self, index: BlockId) -> &mut Block {
        &mut self.blocks[index]
    }
}

#[derive(Clone, Debug, Default)]
pub struct Block {
    pub id: BlockId,
    /// Side-effecting values from the sea-of-nodes that are computed, in order.
    pub insts: Vec<Value>,
    /// Terminator: branch or return.
    pub terminator: Terminator,
    /// Successor blocks.
    pub succs: Vec<BlockId>,
    /// For each successor block, our index in its `preds` array.
    pub pos_in_succ_pred: Vec<usize>,
    /// Predecessor blocks.
    pub preds: Vec<BlockId>,
    /// For each predecessor block, our index in its `succs` array.
    pub pos_in_pred_succ: Vec<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u32);

impl Value {
    pub fn undef() -> Self {
        Value(u32::MAX)
    }

    pub fn index(self) -> usize {
        self.0 as usize
    }

    pub fn from_index(value: usize) -> Value {
        Self(value as u32)
    }
}

impl std::default::Default for Value {
    fn default() -> Self {
        Value::undef()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueDef {
    Arg { index: usize },
    BlockParam { block: BlockId, index: usize },
    Operator { op: Operator, args: Vec<Value> },
    PickOutput { from: Value, index: usize },
}

impl ValueDef {
    pub fn visit_uses<F: FnMut(Value)>(&self, mut f: F) {
        match self {
            &ValueDef::Arg { .. } => {}
            &ValueDef::BlockParam { .. } => {}
            &ValueDef::Operator { ref args, .. } => {
                for &arg in args {
                    f(arg);
                }
            }
            &ValueDef::PickOutput { from, .. } => f(from),
        }
    }

    pub fn update_uses<F: FnMut(&mut Value)>(&mut self, mut f: F) {
        match self {
            &mut ValueDef::Arg { .. } => {}
            &mut ValueDef::BlockParam { .. } => {}
            &mut ValueDef::Operator { ref mut args, .. } => {
                for arg in args {
                    f(arg);
                }
            }
            &mut ValueDef::PickOutput { ref mut from, .. } => f(from),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Terminator {
    Br {
        target: BlockId,
    },
    CondBr {
        cond: Value,
        if_true: BlockId,
        if_false: BlockId,
    },
    Select {
        value: Value,
        targets: Vec<BlockId>,
        default: BlockId,
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
    pub fn visit_successors<F: FnMut(BlockId)>(&self, mut f: F) {
        match self {
            Terminator::Return { .. } => {}
            Terminator::Br { target, .. } => f(*target),
            Terminator::CondBr {
                if_true, if_false, ..
            } => {
                f(*if_true);
                f(*if_false);
            }
            Terminator::Select {
                ref targets,
                default,
                ..
            } => {
                for &target in targets {
                    f(target);
                }
                f(*default);
            }
            Terminator::None => {}
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
