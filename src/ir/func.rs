use super::{Block, Local, Signature, Value, ValueDef};
use crate::entity::EntityVec;
use wasmparser::Type;

#[derive(Clone, Debug)]
pub enum FuncDecl {
    Import(Signature),
    Body(Signature, FunctionBody),
}

impl FuncDecl {
    pub fn sig(&self) -> Signature {
        match self {
            FuncDecl::Import(sig) => *sig,
            FuncDecl::Body(sig, ..) => *sig,
        }
    }

    pub fn body(&self) -> Option<&FunctionBody> {
        match self {
            FuncDecl::Body(_, body) => Some(body),
            _ => None,
        }
    }

    pub fn body_mut(&mut self) -> Option<&mut FunctionBody> {
        match self {
            FuncDecl::Body(_, body) => Some(body),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct FunctionBody {
    /// How many parameters the function has. (Their types are the
    /// first `n_params` values in `locals`.)
    pub n_params: usize,
    /// Return types of the function.
    pub rets: Vec<Type>,
    /// Local types, *including* args.
    pub locals: EntityVec<Local, Type>,
    /// Entry block.
    pub entry: Block,
    /// Block bodies.
    pub blocks: EntityVec<Block, BlockDef>,
    /// Value definitions, indexed by `Value`.
    pub values: EntityVec<Value, ValueDef>,
}

impl FunctionBody {
    pub fn add_block(&mut self) -> Block {
        let id = self.blocks.push(BlockDef::default());
        log::trace!("add_block: block {}", id);
        id
    }

    pub fn add_edge(&mut self, from: Block, to: Block) {
        let succ_pos = self.blocks[from].succs.len();
        let pred_pos = self.blocks[to].preds.len();
        self.blocks[from].succs.push(to);
        self.blocks[to].preds.push(from);
        self.blocks[from].pos_in_succ_pred.push(pred_pos);
        self.blocks[to].pos_in_pred_succ.push(succ_pos);
        log::trace!("add_edge: from {} to {}", from, to);
    }

    pub fn add_value(&mut self, value: ValueDef) -> Value {
        log::trace!("add_value: def {:?}", value);
        self.values.push(value)
    }

    pub fn set_alias(&mut self, value: Value, to: Value) {
        log::trace!("set_alias: value {:?} to {:?}", value, to);
        // Resolve the `to` value through all existing aliases.
        let to = self.resolve_and_update_alias(to);
        // Disallow cycles.
        if to == value {
            panic!("Cannot create an alias cycle");
        }
        self.values[value] = ValueDef::Alias(to);
    }

    pub fn resolve_alias(&self, value: Value) -> Value {
        let mut result = value;
        loop {
            if let &ValueDef::Alias(to) = &self.values[result] {
                result = to;
            } else {
                break;
            }
        }
        result
    }

    pub fn add_mutable_inst(&mut self, def: ValueDef) -> Value {
        let value = Value(self.values.len() as u32);
        self.values.push(def);
        value
    }

    pub fn add_blockparam(&mut self, block: Block, ty: Type) -> Value {
        let index = self.blocks[block].params.len();
        let value = self.add_value(ValueDef::BlockParam(block, index, ty));
        self.blocks[block].params.push((ty, value));
        value
    }

    pub fn add_placeholder(&mut self, ty: Type) -> Value {
        self.add_mutable_inst(ValueDef::Placeholder(ty))
    }

    pub fn replace_placeholder_with_blockparam(&mut self, block: Block, value: Value) {
        let index = self.blocks[block].params.len();
        let ty = match &self.values[value] {
            &ValueDef::Placeholder(ty) => ty,
            _ => unreachable!(),
        };
        self.blocks[block].params.push((ty, value));
        self.values[value] = ValueDef::BlockParam(block, index, ty);
    }

    pub fn resolve_and_update_alias(&mut self, value: Value) -> Value {
        let to = self.resolve_alias(value);
        // Short-circuit the chain, union-find-style.
        if let &ValueDef::Alias(orig_to) = &self.values[value] {
            if orig_to != to {
                self.values[value] = ValueDef::Alias(to);
            }
        }
        to
    }

    pub fn append_to_block(&mut self, block: Block, value: Value) {
        self.blocks[block].insts.push(value);
    }

    pub fn end_block(&mut self, block: Block, terminator: Terminator) {
        terminator.visit_successors(|succ| {
            self.add_edge(block, succ);
        });
        self.blocks[block].terminator = terminator;
    }

    pub fn add_local(&mut self, ty: Type) -> Local {
        self.locals.push(ty)
    }
}

#[derive(Clone, Debug, Default)]
pub struct BlockDef {
    /// Instructions in this block.
    pub insts: Vec<Value>,
    /// Terminator: branch or return.
    pub terminator: Terminator,
    /// Successor blocks.
    pub succs: Vec<Block>,
    /// For each successor block, our index in its `preds` array.
    pub pos_in_succ_pred: Vec<usize>,
    /// Predecessor blocks.
    pub preds: Vec<Block>,
    /// For each predecessor block, our index in its `succs` array.
    pub pos_in_pred_succ: Vec<usize>,
    /// Type and Value for each blockparam.
    pub params: Vec<(Type, Value)>,
}

#[derive(Clone, Debug)]
pub struct BlockTarget {
    pub block: Block,
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
    pub fn visit_targets<F: FnMut(&BlockTarget)>(&self, mut f: F) {
        match self {
            Terminator::Return { .. } => {}
            Terminator::Br { ref target, .. } => f(target),
            Terminator::CondBr {
                ref if_true,
                ref if_false,
                ..
            } => {
                f(if_true);
                f(if_false);
            }
            Terminator::Select {
                ref targets,
                ref default,
                ..
            } => {
                f(default);
                for target in targets {
                    f(target);
                }
            }
            Terminator::None => {}
        }
    }

    pub fn update_targets<F: FnMut(&mut BlockTarget)>(&mut self, mut f: F) {
        match self {
            Terminator::Return { .. } => {}
            Terminator::Br { ref mut target, .. } => f(target),
            Terminator::CondBr {
                ref mut if_true,
                ref mut if_false,
                ..
            } => {
                f(if_true);
                f(if_false);
            }
            Terminator::Select {
                ref mut targets,
                ref mut default,
                ..
            } => {
                f(default);
                for target in targets {
                    f(target);
                }
            }
            Terminator::None => {}
        }
    }

    pub fn visit_target<F: FnMut(&BlockTarget)>(&self, index: usize, mut f: F) {
        match (index, self) {
            (0, Terminator::Br { ref target, .. }) => f(target),
            (0, Terminator::CondBr { ref if_true, .. }) => {
                f(if_true);
            }
            (1, Terminator::CondBr { ref if_false, .. }) => {
                f(if_false);
            }
            (0, Terminator::Select { ref default, .. }) => {
                f(default);
            }
            (i, Terminator::Select { ref targets, .. }) if i <= targets.len() => {
                f(&targets[i - 1]);
            }
            _ => panic!("out of bounds"),
        }
    }

    pub fn update_target<F: FnMut(&mut BlockTarget)>(&mut self, index: usize, mut f: F) {
        match (index, self) {
            (0, Terminator::Br { ref mut target, .. }) => f(target),
            (
                0,
                Terminator::CondBr {
                    ref mut if_true, ..
                },
            ) => {
                f(if_true);
            }
            (
                1,
                Terminator::CondBr {
                    ref mut if_false, ..
                },
            ) => {
                f(if_false);
            }
            (
                0,
                Terminator::Select {
                    ref mut default, ..
                },
            ) => {
                f(default);
            }
            (
                i,
                Terminator::Select {
                    ref mut targets, ..
                },
            ) if i <= targets.len() => {
                f(&mut targets[i - 1]);
            }
            (i, this) => panic!("out of bounds: index {} term {:?}", i, this),
        }
    }

    pub fn visit_successors<F: FnMut(Block)>(&self, mut f: F) {
        self.visit_targets(|target| f(target.block));
    }

    pub fn visit_uses<F: FnMut(Value)>(&self, mut f: F) {
        self.visit_targets(|target| {
            for &arg in &target.args {
                f(arg);
            }
        });
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
        self.update_targets(|target| {
            for arg in &mut target.args {
                f(arg);
            }
        });
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
