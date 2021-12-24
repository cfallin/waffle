//! Intermediate representation for Wasm.

use crate::{
    backend::{
        produce_func_wasm, BlockOrder, FuncTypeSink, Locations, LoopNest, SerializedBody,
        WasmRegion,
    },
    cfg::CFGInfo,
    frontend,
    ops::ty_to_valty,
    Operator,
};
use anyhow::Result;
use fxhash::FxHashMap;
use rayon::prelude::*;
use std::collections::hash_map::Entry;
use wasmparser::{FuncType, SectionReader, Type};

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
    pub rets: Vec<Type>,
    pub locals: Vec<Type>,
    pub blocks: Vec<Block>,
    /// Sea-of-nodes representation.
    pub values: Vec<ValueDef>,
    value_dedup: FxHashMap<ValueDef, Value>,
    /// A single value can have multiple types if multi-value (e.g. a
    /// call).
    pub types: Vec</* Value, */ Vec<Type>>,
}

impl FunctionBody {
    pub fn add_block(&mut self) -> BlockId {
        let id = self.blocks.len();
        self.blocks.push(Block::default());
        self.blocks[id].id = id;
        log::trace!("add_block: block {}", id);
        id
    }

    pub fn add_edge(&mut self, from: BlockId, to: BlockId) {
        let succ_pos = self.blocks[from].succs.len();
        let pred_pos = self.blocks[to].preds.len();
        self.blocks[from].succs.push(to);
        self.blocks[to].preds.push(from);
        self.blocks[from].pos_in_succ_pred.push(pred_pos);
        self.blocks[to].pos_in_pred_succ.push(succ_pos);
        log::trace!("add_edge: from {} to {}", from, to);
    }

    pub fn add_value(&mut self, value: ValueDef, tys: Vec<Type>) -> Value {
        log::trace!("add_value: def {:?} ty {:?}", value, tys);
        let id = match self.value_dedup.entry(value.clone()) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let id = Value(self.values.len() as u32);
                self.values.push(value.clone());
                self.types.push(tys);
                v.insert(id);
                id
            }
        };
        log::trace!(" -> value {:?}", id);
        id
    }

    pub fn set_alias(&mut self, value: Value, to: Value) {
        log::trace!("set_alias: value {:?} to {:?}", value, to);
        // Resolve the `to` value through all existing aliases.
        let to = self.resolve_and_update_alias(to);
        // Disallow cycles.
        if to == value {
            panic!("Cannot create an alias cycle");
        }
        self.values[value.index()] = ValueDef::Alias(to);
    }

    pub fn resolve_alias(&self, value: Value) -> Value {
        let mut result = value;
        loop {
            if let &ValueDef::Alias(to) = &self.values[result.index()] {
                result = to;
            } else {
                break;
            }
        }
        result
    }

    pub fn add_mutable_inst(&mut self, tys: Vec<Type>, def: ValueDef) -> Value {
        let value = Value(self.values.len() as u32);
        self.types.push(tys);
        self.values.push(def);
        value
    }

    pub fn add_blockparam(&mut self, block: BlockId, ty: Type) -> Value {
        let index = self.blocks[block].params.len();
        let value = self.add_value(ValueDef::BlockParam(block, index), vec![ty]);
        self.blocks[block].params.push((ty, value));
        value
    }

    pub fn add_placeholder(&mut self, ty: Type) -> Value {
        self.add_mutable_inst(vec![ty], ValueDef::Placeholder)
    }

    pub fn replace_placeholder_with_blockparam(&mut self, block: BlockId, value: Value) {
        assert!(self.values[value.index()] == ValueDef::Placeholder);
        let ty = self.types[value.index()].get(0).cloned().unwrap();
        let index = self.blocks[block].params.len();
        self.blocks[block].params.push((ty, value));
        self.values[value.index()] = ValueDef::BlockParam(block, index);
    }

    pub fn resolve_and_update_alias(&mut self, value: Value) -> Value {
        let to = self.resolve_alias(value);
        // Short-circuit the chain, union-find-style.
        if let &ValueDef::Alias(orig_to) = &self.values[value.index()] {
            if orig_to != to {
                self.values[value.index()] = ValueDef::Alias(to);
            }
        }
        to
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

    pub fn values<'a>(&'a self) -> impl Iterator<Item = (Value, &'a ValueDef)> + 'a {
        self.values
            .iter()
            .enumerate()
            .map(|(idx, value_def)| (Value(idx as u32), value_def))
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
    /// Type and Value for each blockparam.
    pub params: Vec<(Type, Value)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u32);

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl Value {
    pub fn index(self) -> usize {
        self.0 as usize
    }

    pub fn from_index(value: usize) -> Value {
        Self(value as u32)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueDef {
    Arg(usize),
    BlockParam(BlockId, usize),
    Operator(Operator, Vec<Value>),
    PickOutput(Value, usize),
    Alias(Value),
    Placeholder,
}

impl ValueDef {
    pub fn visit_uses<F: FnMut(Value)>(&self, mut f: F) {
        match self {
            &ValueDef::Arg { .. } => {}
            &ValueDef::BlockParam { .. } => {}
            &ValueDef::Operator(_, ref args) => {
                for &arg in args {
                    f(arg);
                }
            }
            &ValueDef::PickOutput(from, ..) => f(from),
            &ValueDef::Alias(value) => f(value),
            &ValueDef::Placeholder => {}
        }
    }

    pub fn update_uses<F: FnMut(&mut Value)>(&mut self, mut f: F) {
        match self {
            &mut ValueDef::Arg { .. } => {}
            &mut ValueDef::BlockParam { .. } => {}
            &mut ValueDef::Operator(_, ref mut args) => {
                for arg in args {
                    f(arg);
                }
            }
            &mut ValueDef::PickOutput(ref mut from, ..) => f(from),
            &mut ValueDef::Alias(ref mut value) => f(value),
            &mut ValueDef::Placeholder => {}
        }
    }
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

    pub fn visit_successors<F: FnMut(BlockId)>(&self, mut f: F) {
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

impl<'a> Module<'a> {
    pub fn from_wasm_bytes(bytes: &'a [u8]) -> Result<Self> {
        frontend::wasm_to_ir(bytes)
    }

    pub fn to_wasm_bytes(&self) -> Vec<u8> {
        // Do most of the compilation in parallel: up to the
        // serialized (pre-regalloc) body and the regalloc
        // results. Only the "final parts assembly" needs to be
        // serialized because it can add function signatures.
        let compiled: Vec<(u32, &FunctionBody, SerializedBody, Locations)> = self
            .funcs
            .par_iter()
            .filter_map(|func| match func {
                &FuncDecl::Body(sig, ref body) => {
                    let cfg = CFGInfo::new(body);
                    let loopnest = LoopNest::compute(&cfg);
                    let regions = WasmRegion::compute(&cfg, &loopnest);
                    let blockorder = BlockOrder::compute(body, &cfg, &regions);
                    let serialized = SerializedBody::compute(body, &cfg, &blockorder);
                    log::trace!("serialized: {:?}", serialized);
                    let locations = Locations::compute(body, &serialized);
                    log::trace!("locations: {:?}", locations);
                    Some((sig as u32, body, serialized, locations))
                }
                _ => None,
            })
            .collect();

        // Build the final code section and function-type section.
        let mut signatures = SignatureAdder::new(&self);
        let mut code_section = wasm_encoder::CodeSection::new();
        let mut func_section = wasm_encoder::FunctionSection::new();
        for (sig, body, serialized, locations) in compiled {
            let func_body = produce_func_wasm(body, &serialized, &locations, &mut signatures);
            log::trace!("body: {:?}", func_body);

            let mut locals: Vec<(u32, wasm_encoder::ValType)> = vec![];
            for local_ty in func_body.locals {
                if locals.len() > 0 && locals.last().unwrap().1 == local_ty {
                    locals.last_mut().unwrap().0 += 1;
                } else {
                    locals.push((1, local_ty));
                }
            }
            let mut func = wasm_encoder::Function::new(locals);

            for inst in func_body.operators {
                func.instruction(&inst);
            }

            func_section.function(sig);
            code_section.function(&func);
        }

        // Build the final function-signature (type) section.
        let mut type_section = wasm_encoder::TypeSection::new();
        for sig in &signatures.signatures {
            let params: Vec<wasm_encoder::ValType> =
                sig.params.iter().map(|&ty| ty_to_valty(ty)).collect();
            let returns: Vec<wasm_encoder::ValType> =
                sig.returns.iter().map(|&ty| ty_to_valty(ty)).collect();
            type_section.function(params, returns);
        }

        // Now do a final pass over the original bytes with
        // wasmparser, replacing the type section, function section,
        // and code section. (TODO: allow new imports to be added
        // too?)
        let parser = wasmparser::Parser::new(0);
        let mut module = wasm_encoder::Module::new();
        for payload in parser.parse_all(self.orig_bytes) {
            match payload.unwrap() {
                wasmparser::Payload::TypeSection(..) => {
                    module.section(&type_section);
                }
                wasmparser::Payload::FunctionSection(..) => {
                    module.section(&func_section);
                }
                wasmparser::Payload::CodeSectionStart { .. } => {
                    module.section(&code_section);
                }
                wasmparser::Payload::CodeSectionEntry(..) => {}
                wasmparser::Payload::ImportSection(reader) => {
                    let range = reader.range();
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection { id: 2, data: bytes });
                }
                wasmparser::Payload::TableSection(reader) => {
                    let range = reader.range();
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection { id: 4, data: bytes });
                }
                wasmparser::Payload::MemorySection(reader) => {
                    let range = reader.range();
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection { id: 5, data: bytes });
                }
                wasmparser::Payload::GlobalSection(reader) => {
                    let range = reader.range();
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection { id: 6, data: bytes });
                }
                wasmparser::Payload::ExportSection(reader) => {
                    let range = reader.range();
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection { id: 7, data: bytes });
                }
                wasmparser::Payload::StartSection { range, .. } => {
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection { id: 8, data: bytes });
                }
                wasmparser::Payload::ElementSection(reader) => {
                    let range = reader.range();
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection { id: 9, data: bytes });
                }
                wasmparser::Payload::DataSection(reader) => {
                    let range = reader.range();
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection {
                        id: 11,
                        data: bytes,
                    });
                }
                wasmparser::Payload::DataCountSection { range, .. } => {
                    let bytes = &self.orig_bytes[range.start..range.end];
                    module.section(&wasm_encoder::RawSection {
                        id: 12,
                        data: bytes,
                    });
                }
                _ => {}
            }
        }

        module.finish()
    }
}

struct SignatureAdder {
    signatures: Vec<FuncType>,
    signature_dedup: FxHashMap<FuncType, u32>,
}

impl SignatureAdder {
    fn new(module: &Module<'_>) -> Self {
        let signature_dedup: FxHashMap<FuncType, u32> = module
            .signatures
            .iter()
            .enumerate()
            .map(|(idx, sig)| (sig.clone(), idx as u32))
            .collect();

        Self {
            signatures: module.signatures.clone(),
            signature_dedup,
        }
    }
}

impl FuncTypeSink for SignatureAdder {
    fn add_signature(&mut self, params: Vec<Type>, results: Vec<Type>) -> u32 {
        let ft = wasmparser::FuncType {
            params: params.into_boxed_slice(),
            returns: results.into_boxed_slice(),
        };
        match self.signature_dedup.entry(ft.clone()) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let idx = self.signatures.len() as u32;
                self.signatures.push(ft);
                *v.insert(idx)
            }
        }
    }
}
