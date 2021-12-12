//! Frontend: convert Wasm to IR.

#![allow(dead_code)]

use std::convert::TryFrom;

use crate::ir::*;
use crate::op_traits::{op_effects, op_inputs, op_outputs};
use crate::ops::Operator;
use anyhow::{bail, Result};
use fxhash::{FxHashMap, FxHashSet};
use log::trace;
use wasmparser::{
    Ieee32, Ieee64, ImportSectionEntryType, Parser, Payload, Type, TypeDef, TypeOrFuncType,
};

pub fn wasm_to_ir(bytes: &[u8]) -> Result<Module<'_>> {
    let mut module = Module::default();
    module.orig_bytes = bytes;
    let parser = Parser::new(0);
    let mut next_func = 0;
    for payload in parser.parse_all(bytes) {
        let payload = payload?;
        handle_payload(&mut module, payload, &mut next_func)?;
    }

    Ok(module)
}

fn handle_payload<'a>(
    module: &mut Module<'a>,
    payload: Payload<'a>,
    next_func: &mut usize,
) -> Result<()> {
    trace!("Wasm parser item: {:?}", payload);
    match payload {
        Payload::TypeSection(mut reader) => {
            for _ in 0..reader.get_count() {
                let ty = reader.read()?;
                if let TypeDef::Func(fty) = ty {
                    module.signatures.push(fty);
                }
            }
        }
        Payload::ImportSection(mut reader) => {
            for _ in 0..reader.get_count() {
                match reader.read()?.ty {
                    ImportSectionEntryType::Function(sig_idx) => {
                        module.funcs.push(FuncDecl::Import(sig_idx as SignatureId));
                        *next_func += 1;
                    }
                    ImportSectionEntryType::Global(ty) => {
                        module.globals.push(ty.content_type);
                    }
                    ImportSectionEntryType::Table(ty) => {
                        module.tables.push(ty.element_type);
                    }
                    _ => {}
                }
            }
        }
        Payload::GlobalSection(mut reader) => {
            for _ in 0..reader.get_count() {
                let global = reader.read()?;
                module.globals.push(global.ty.content_type);
            }
        }
        Payload::TableSection(mut reader) => {
            for _ in 0..reader.get_count() {
                let table = reader.read()?;
                module.tables.push(table.element_type);
            }
        }
        Payload::FunctionSection(mut reader) => {
            for _ in 0..reader.get_count() {
                let sig_idx = reader.read()? as SignatureId;
                module
                    .funcs
                    .push(FuncDecl::Body(sig_idx, FunctionBody::default()));
            }
        }
        Payload::CodeSectionEntry(body) => {
            let func_idx = *next_func;
            *next_func += 1;

            let my_sig = module.funcs[func_idx].sig();
            let body = parse_body(module, my_sig, body)?;

            match &mut module.funcs[func_idx] {
                FuncDecl::Body(_, ref mut existing_body) => {
                    *existing_body = body;
                }
                _ => unreachable!(),
            }
        }
        _ => {}
    }

    Ok(())
}

fn parse_body<'a>(
    module: &'a Module,
    my_sig: SignatureId,
    body: wasmparser::FunctionBody,
) -> Result<FunctionBody> {
    let mut ret: FunctionBody = FunctionBody::default();

    for &param in &module.signatures[my_sig].params[..] {
        ret.locals.push(param);
    }

    let mut locals = body.get_locals_reader()?;
    for _ in 0..locals.get_count() {
        let (count, ty) = locals.read()?;
        for _ in 0..count {
            ret.locals.push(ty);
        }
    }
    let locals = ret.locals.clone();

    trace!(
        "Parsing function body: locals = {:?} sig = {:?}",
        ret.locals,
        module.signatures[my_sig]
    );

    let mut builder = FunctionBodyBuilder::new(module, my_sig, &mut ret);
    builder.locals.seal_block_preds(0, &mut builder.body);
    builder.locals.start_block(0);

    for (arg_idx, &arg_ty) in module.signatures[my_sig].params.iter().enumerate() {
        let local_idx = arg_idx as LocalId;
        let value = builder.body.add_value(ValueDef::Arg(arg_idx), Some(arg_ty));
        trace!("defining local {} to value {}", local_idx, value);
        builder.locals.declare(local_idx, arg_ty);
        builder.locals.set(local_idx, value);
    }

    let n_args = module.signatures[my_sig].params.len();
    for (offset, local_ty) in locals.into_iter().enumerate() {
        let local_idx = (n_args + offset) as u32;
        builder.locals.declare(local_idx, local_ty);
    }

    let ops = body.get_operators_reader()?;
    for op in ops.into_iter() {
        let op = op?;
        builder.handle_op(op)?;
    }

    if builder.cur_block.is_some() {
        builder.handle_op(wasmparser::Operator::Return)?;
    }

    for block in 0..builder.body.blocks.len() {
        log::trace!("checking if block is sealed: {}", block);
        assert!(builder.locals.is_sealed(block));
    }
    for value in &builder.body.values {
        assert!(value != &ValueDef::Placeholder);
    }

    trace!("Final function body:{:?}", ret);

    Ok(ret)
}

#[derive(Debug, Clone, Default)]
struct LocalTracker {
    /// Types of locals, as declared.
    types: FxHashMap<LocalId, Type>,
    /// The current block.
    cur_block: Option<BlockId>,
    /// Is the given block sealed?
    block_sealed: FxHashSet<BlockId>,
    /// The local-to-value mapping at the start of a block.
    block_start: FxHashMap<BlockId, FxHashMap<LocalId, Value>>,
    /// The local-to-value mapping at the end of a block.
    block_end: FxHashMap<BlockId, FxHashMap<LocalId, Value>>,
    in_cur_block: FxHashMap<LocalId, Value>,
    incomplete_phis: FxHashMap<BlockId, Vec<(LocalId, Value)>>,
}

impl LocalTracker {
    pub fn declare(&mut self, local: LocalId, ty: Type) {
        let was_present = self.types.insert(local, ty).is_some();
        assert!(!was_present);
    }

    pub fn start_block(&mut self, block: BlockId) {
        self.finish_block();
        log::trace!("start_block: block {}", block);
        self.cur_block = Some(block);
    }

    pub fn finish_block(&mut self) {
        log::trace!("finish_block: block {:?}", self.cur_block);
        if let Some(block) = self.cur_block {
            let mapping = std::mem::take(&mut self.in_cur_block);
            self.block_end.insert(block, mapping);
        }
        self.cur_block = None;
    }

    pub fn seal_block_preds(&mut self, block: BlockId, body: &mut FunctionBody) {
        log::trace!("seal_block_preds: block {}", block);
        let not_sealed = self.block_sealed.insert(block);
        assert!(not_sealed);
        for (local, phi_value) in self
            .incomplete_phis
            .remove(&block)
            .unwrap_or_else(|| vec![])
        {
            self.compute_blockparam(body, block, local, phi_value);
        }
    }

    fn is_sealed(&self, block: BlockId) -> bool {
        self.block_sealed.contains(&block)
    }

    pub fn set(&mut self, local: LocalId, value: Value) {
        if self.cur_block.is_none() {
            return;
        }

        log::trace!("set: local {} value {:?}", local, value);
        self.in_cur_block.insert(local, value);
    }

    fn get_in_block(
        &mut self,
        body: &mut FunctionBody,
        at_block: BlockId,
        local: LocalId,
    ) -> Value {
        log::trace!("get_in_block: at_block {} local {}", at_block, local);
        let ty = body.locals[local as usize];

        if self.cur_block == Some(at_block) {
            if let Some(&value) = self.in_cur_block.get(&local) {
                log::trace!(" -> {:?}", value);
                return value;
            }
        }

        if self.is_sealed(at_block) {
            if let Some(end_mapping) = self.block_end.get(&at_block) {
                if let Some(&value) = end_mapping.get(&local) {
                    log::trace!(" -> {:?}", value);
                    return value;
                }
            }

            if body.blocks[at_block].preds.is_empty() {
                let value = self.create_default_value(body, ty);
                log::trace!(" -> created default: {:?}", value);
                return value;
            }

            let placeholder = body.add_placeholder(ty);
            self.block_end
                .entry(at_block)
                .or_insert_with(|| FxHashMap::default())
                .insert(local, placeholder);
            log::trace!(" -> created placeholder: {:?}", placeholder);
            self.compute_blockparam(body, at_block, local, placeholder);
            placeholder
        } else {
            let placeholder = body.add_placeholder(ty);
            self.block_end
                .entry(at_block)
                .or_insert_with(|| FxHashMap::default())
                .insert(local, placeholder);
            log::trace!(
                " -> created placeholder and added as incomplete phi: {:?}",
                placeholder
            );
            self.incomplete_phis
                .entry(at_block)
                .or_insert_with(|| vec![])
                .push((local, placeholder));

            placeholder
        }
    }

    pub fn get(&mut self, body: &mut FunctionBody, local: LocalId) -> Value {
        if self.cur_block.is_none() {
            return Value::undef();
        }

        let block = self.cur_block.unwrap();
        assert!((local as usize) < body.locals.len());
        self.get_in_block(body, block, local)
    }

    fn create_default_value(&mut self, body: &mut FunctionBody, ty: Type) -> Value {
        match ty {
            Type::I32 => body.add_value(
                ValueDef::Operator(Operator::I32Const { value: 0 }, vec![]),
                Some(ty),
            ),
            Type::I64 => body.add_value(
                ValueDef::Operator(Operator::I64Const { value: 0 }, vec![]),
                Some(ty),
            ),
            Type::F32 => body.add_value(
                ValueDef::Operator(
                    Operator::F32Const {
                        value: Ieee32::from_bits(0),
                    },
                    vec![],
                ),
                Some(ty),
            ),
            Type::F64 => body.add_value(
                ValueDef::Operator(
                    Operator::F64Const {
                        value: Ieee64::from_bits(0),
                    },
                    vec![],
                ),
                Some(ty),
            ),
            _ => todo!("unsupported type: {:?}", ty),
        }
    }

    fn compute_blockparam(
        &mut self,
        body: &mut FunctionBody,
        block: BlockId,
        local: LocalId,
        value: Value,
    ) {
        log::trace!(
            "compute_blockparam: block {} local {} value {:?}",
            block,
            local,
            value
        );
        let mut results: Vec<Value> = vec![];
        let preds = body.blocks[block].preds.clone();
        for pred in preds {
            let pred_value = self.get_in_block(body, pred, local);
            log::trace!(
                "compute_blockparam: block {} local {} value {:?}: pred {} -> {:?}",
                block,
                local,
                value,
                pred,
                pred_value
            );
            results.push(pred_value);
        }

        let mut non_self = results.iter().filter(|&&v| v != value);
        let trivial_alias = match non_self.next() {
            None => None,
            Some(&first) if non_self.all(|&v| v == first) && body.resolve_alias(first) != value => {
                Some(first)
            }
            Some(_) => None,
        };

        if let Some(v) = trivial_alias {
            log::trace!(
                "compute_blockparam: block {} local {} value {:?}: alias to {:?}",
                block,
                local,
                value,
                v
            );
            body.set_alias(value, v);
        } else {
            log::trace!(
                "compute_blockparam: block {} local {} value {:?}: making blockparam",
                block,
                local,
                value,
            );
            body.replace_placeholder_with_blockparam(block, value);
            for (i, (&pred, result)) in body.blocks[block]
                .preds
                .clone()
                .iter()
                .zip(results.into_iter())
                .enumerate()
            {
                let index = body.blocks[block].pos_in_pred_succ[i];
                body.blocks[pred].terminator.update_target(index, |target| {
                    log::trace!(
                        "compute_blockparam: block {} local {} value {:?}: in pred {}, adding branch arg {:?}",
                        block,
                        local,
                        value,
                        pred,
                        result,
                    );
                    target.args.push(result);
                });
            }
        }
    }
}

#[derive(Debug)]
struct FunctionBodyBuilder<'a, 'b> {
    module: &'b Module<'a>,
    my_sig: SignatureId,
    body: &'b mut FunctionBody,
    locals: LocalTracker,
    cur_block: Option<BlockId>,
    ctrl_stack: Vec<Frame>,
    op_stack: Vec<(Type, Value)>,
}

#[derive(Clone, Debug)]
enum Frame {
    Block {
        start_depth: usize,
        out: BlockId,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    Loop {
        start_depth: usize,
        header: BlockId,
        out: BlockId,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    If {
        start_depth: usize,
        out: BlockId,
        el: BlockId,
        param_values: Vec<(Type, Value)>,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    Else {
        start_depth: usize,
        out: BlockId,
        params: Vec<Type>,
        results: Vec<Type>,
    },
}

impl Frame {
    fn start_depth(&self) -> usize {
        match self {
            Frame::Block { start_depth, .. }
            | Frame::Loop { start_depth, .. }
            | Frame::If { start_depth, .. }
            | Frame::Else { start_depth, .. } => *start_depth,
        }
    }

    fn br_args(&self) -> &[Type] {
        match self {
            Frame::Block { results, .. }
            | Frame::If { results, .. }
            | Frame::Else { results, .. } => &results[..],
            Frame::Loop { params, .. } => &params[..],
        }
    }

    fn br_target(&self) -> BlockId {
        match self {
            Frame::Block { out, .. } => *out,
            Frame::Loop { header, .. } => *header,
            Frame::If { out, .. } | Frame::Else { out, .. } => *out,
        }
    }

    fn out(&self) -> BlockId {
        match self {
            Frame::Block { out, .. }
            | Frame::Loop { out, .. }
            | Frame::If { out, .. }
            | Frame::Else { out, .. } => *out,
        }
    }

    fn params(&self) -> &[Type] {
        match self {
            Frame::Block { params, .. }
            | Frame::Loop { params, .. }
            | Frame::If { params, .. }
            | Frame::Else { params, .. } => &params[..],
        }
    }

    fn results(&self) -> &[Type] {
        match self {
            Frame::Block { results, .. }
            | Frame::Loop { results, .. }
            | Frame::If { results, .. }
            | Frame::Else { results, .. } => &results[..],
        }
    }
}

impl<'a, 'b> FunctionBodyBuilder<'a, 'b> {
    fn new(module: &'b Module<'a>, my_sig: SignatureId, body: &'b mut FunctionBody) -> Self {
        body.blocks.push(Block::default());
        let mut ret = Self {
            module,
            my_sig,
            body,
            ctrl_stack: vec![],
            op_stack: vec![],
            cur_block: Some(0),
            locals: LocalTracker::default(),
        };

        // Push initial implicit Block.
        let results = module.signatures[my_sig].returns.to_vec();
        let out = ret.body.add_block();
        ret.add_block_params(out, &results[..]);
        ret.ctrl_stack.push(Frame::Block {
            start_depth: 0,
            out,
            params: vec![],
            results,
        });
        ret
    }

    fn pop_n(&mut self, n: usize) -> Vec<Value> {
        let new_top = self.op_stack.len() - n;
        let ret = self.op_stack[new_top..]
            .iter()
            .map(|(_ty, value)| *value)
            .collect::<Vec<_>>();
        self.op_stack.truncate(new_top);
        ret
    }

    fn pop_1(&mut self) -> Value {
        self.op_stack.pop().unwrap().1
    }

    fn handle_op(&mut self, op: wasmparser::Operator<'a>) -> Result<()> {
        trace!("handle_op: {:?}", op);
        trace!("op_stack = {:?}", self.op_stack);
        trace!("ctrl_stack = {:?}", self.ctrl_stack);
        trace!("locals = {:?}", self.locals);
        match &op {
            wasmparser::Operator::Unreachable => {
                if let Some(block) = self.cur_block {
                    self.body.blocks[block].terminator = Terminator::None;
                    self.locals.finish_block();
                }
                self.cur_block = None;
            }

            wasmparser::Operator::LocalGet { local_index } => {
                let ty = self.body.locals[*local_index as usize];
                let value = self.locals.get(&mut self.body, *local_index);
                self.op_stack.push((ty, value));
            }

            wasmparser::Operator::LocalSet { local_index } => {
                let (_, value) = self.op_stack.pop().unwrap();
                self.locals.set(*local_index, value);
            }

            wasmparser::Operator::LocalTee { local_index } => {
                let (_ty, value) = *self.op_stack.last().unwrap();
                self.locals.set(*local_index, value);
            }

            wasmparser::Operator::Call { .. }
            | wasmparser::Operator::CallIndirect { .. }
            | wasmparser::Operator::Select
            | wasmparser::Operator::TypedSelect { .. }
            | wasmparser::Operator::GlobalGet { .. }
            | wasmparser::Operator::GlobalSet { .. }
            | wasmparser::Operator::I32Load { .. }
            | wasmparser::Operator::I64Load { .. }
            | wasmparser::Operator::F32Load { .. }
            | wasmparser::Operator::F64Load { .. }
            | wasmparser::Operator::I32Load8S { .. }
            | wasmparser::Operator::I32Load8U { .. }
            | wasmparser::Operator::I32Load16S { .. }
            | wasmparser::Operator::I32Load16U { .. }
            | wasmparser::Operator::I64Load8S { .. }
            | wasmparser::Operator::I64Load8U { .. }
            | wasmparser::Operator::I64Load16S { .. }
            | wasmparser::Operator::I64Load16U { .. }
            | wasmparser::Operator::I64Load32S { .. }
            | wasmparser::Operator::I64Load32U { .. }
            | wasmparser::Operator::I32Store { .. }
            | wasmparser::Operator::I64Store { .. }
            | wasmparser::Operator::F32Store { .. }
            | wasmparser::Operator::F64Store { .. }
            | wasmparser::Operator::I32Store8 { .. }
            | wasmparser::Operator::I32Store16 { .. }
            | wasmparser::Operator::I64Store8 { .. }
            | wasmparser::Operator::I64Store16 { .. }
            | wasmparser::Operator::I64Store32 { .. }
            | wasmparser::Operator::MemorySize { .. }
            | wasmparser::Operator::MemoryGrow { .. }
            | wasmparser::Operator::I32Const { .. }
            | wasmparser::Operator::I64Const { .. }
            | wasmparser::Operator::F32Const { .. }
            | wasmparser::Operator::F64Const { .. }
            | wasmparser::Operator::I32Eqz
            | wasmparser::Operator::I32Eq
            | wasmparser::Operator::I32Ne
            | wasmparser::Operator::I32LtS
            | wasmparser::Operator::I32LtU
            | wasmparser::Operator::I32GtS
            | wasmparser::Operator::I32GtU
            | wasmparser::Operator::I32LeS
            | wasmparser::Operator::I32LeU
            | wasmparser::Operator::I32GeS
            | wasmparser::Operator::I32GeU
            | wasmparser::Operator::I64Eqz
            | wasmparser::Operator::I64Eq
            | wasmparser::Operator::I64Ne
            | wasmparser::Operator::I64LtS
            | wasmparser::Operator::I64LtU
            | wasmparser::Operator::I64GtU
            | wasmparser::Operator::I64GtS
            | wasmparser::Operator::I64LeS
            | wasmparser::Operator::I64LeU
            | wasmparser::Operator::I64GeS
            | wasmparser::Operator::I64GeU
            | wasmparser::Operator::F32Eq
            | wasmparser::Operator::F32Ne
            | wasmparser::Operator::F32Lt
            | wasmparser::Operator::F32Gt
            | wasmparser::Operator::F32Le
            | wasmparser::Operator::F32Ge
            | wasmparser::Operator::F64Eq
            | wasmparser::Operator::F64Ne
            | wasmparser::Operator::F64Lt
            | wasmparser::Operator::F64Gt
            | wasmparser::Operator::F64Le
            | wasmparser::Operator::F64Ge
            | wasmparser::Operator::I32Clz
            | wasmparser::Operator::I32Ctz
            | wasmparser::Operator::I32Popcnt
            | wasmparser::Operator::I32Add
            | wasmparser::Operator::I32Sub
            | wasmparser::Operator::I32Mul
            | wasmparser::Operator::I32DivS
            | wasmparser::Operator::I32DivU
            | wasmparser::Operator::I32RemS
            | wasmparser::Operator::I32RemU
            | wasmparser::Operator::I32And
            | wasmparser::Operator::I32Or
            | wasmparser::Operator::I32Xor
            | wasmparser::Operator::I32Shl
            | wasmparser::Operator::I32ShrS
            | wasmparser::Operator::I32ShrU
            | wasmparser::Operator::I32Rotl
            | wasmparser::Operator::I32Rotr
            | wasmparser::Operator::I64Clz
            | wasmparser::Operator::I64Ctz
            | wasmparser::Operator::I64Popcnt
            | wasmparser::Operator::I64Add
            | wasmparser::Operator::I64Sub
            | wasmparser::Operator::I64Mul
            | wasmparser::Operator::I64DivS
            | wasmparser::Operator::I64DivU
            | wasmparser::Operator::I64RemS
            | wasmparser::Operator::I64RemU
            | wasmparser::Operator::I64And
            | wasmparser::Operator::I64Or
            | wasmparser::Operator::I64Xor
            | wasmparser::Operator::I64Shl
            | wasmparser::Operator::I64ShrS
            | wasmparser::Operator::I64ShrU
            | wasmparser::Operator::I64Rotl
            | wasmparser::Operator::I64Rotr
            | wasmparser::Operator::F32Abs
            | wasmparser::Operator::F32Neg
            | wasmparser::Operator::F32Ceil
            | wasmparser::Operator::F32Floor
            | wasmparser::Operator::F32Trunc
            | wasmparser::Operator::F32Nearest
            | wasmparser::Operator::F32Sqrt
            | wasmparser::Operator::F32Add
            | wasmparser::Operator::F32Sub
            | wasmparser::Operator::F32Mul
            | wasmparser::Operator::F32Div
            | wasmparser::Operator::F32Min
            | wasmparser::Operator::F32Max
            | wasmparser::Operator::F32Copysign
            | wasmparser::Operator::F64Abs
            | wasmparser::Operator::F64Neg
            | wasmparser::Operator::F64Ceil
            | wasmparser::Operator::F64Floor
            | wasmparser::Operator::F64Trunc
            | wasmparser::Operator::F64Nearest
            | wasmparser::Operator::F64Sqrt
            | wasmparser::Operator::F64Add
            | wasmparser::Operator::F64Sub
            | wasmparser::Operator::F64Mul
            | wasmparser::Operator::F64Div
            | wasmparser::Operator::F64Min
            | wasmparser::Operator::F64Max
            | wasmparser::Operator::F64Copysign
            | wasmparser::Operator::I32WrapI64
            | wasmparser::Operator::I32TruncF32S
            | wasmparser::Operator::I32TruncF32U
            | wasmparser::Operator::I32TruncF64S
            | wasmparser::Operator::I32TruncF64U
            | wasmparser::Operator::I64ExtendI32S
            | wasmparser::Operator::I64ExtendI32U
            | wasmparser::Operator::I64TruncF32S
            | wasmparser::Operator::I64TruncF32U
            | wasmparser::Operator::I64TruncF64S
            | wasmparser::Operator::I64TruncF64U
            | wasmparser::Operator::F32ConvertI32S
            | wasmparser::Operator::F32ConvertI32U
            | wasmparser::Operator::F32ConvertI64S
            | wasmparser::Operator::F32ConvertI64U
            | wasmparser::Operator::F32DemoteF64
            | wasmparser::Operator::F64ConvertI32S
            | wasmparser::Operator::F64ConvertI32U
            | wasmparser::Operator::F64ConvertI64S
            | wasmparser::Operator::F64ConvertI64U
            | wasmparser::Operator::F64PromoteF32
            | wasmparser::Operator::I32Extend8S
            | wasmparser::Operator::I32Extend16S
            | wasmparser::Operator::I64Extend8S
            | wasmparser::Operator::I64Extend16S
            | wasmparser::Operator::I64Extend32S
            | wasmparser::Operator::I32TruncSatF32S
            | wasmparser::Operator::I32TruncSatF32U
            | wasmparser::Operator::I32TruncSatF64S
            | wasmparser::Operator::I32TruncSatF64U
            | wasmparser::Operator::I64TruncSatF32S
            | wasmparser::Operator::I64TruncSatF32U
            | wasmparser::Operator::I64TruncSatF64S
            | wasmparser::Operator::I64TruncSatF64U
            | wasmparser::Operator::F32ReinterpretI32
            | wasmparser::Operator::F64ReinterpretI64
            | wasmparser::Operator::I32ReinterpretF32
            | wasmparser::Operator::I64ReinterpretF64
            | wasmparser::Operator::TableGet { .. }
            | wasmparser::Operator::TableSet { .. }
            | wasmparser::Operator::TableGrow { .. }
            | wasmparser::Operator::TableSize { .. } => {
                self.emit(Operator::try_from(&op).unwrap())?
            }

            wasmparser::Operator::Nop => {}

            wasmparser::Operator::Drop => {
                let _ = self.pop_1();
            }

            wasmparser::Operator::End if self.cur_block.is_none() => {
                let frame = self.ctrl_stack.pop().unwrap();
                self.op_stack.truncate(frame.start_depth());
                match frame {
                    Frame::Block { out, .. } | Frame::If { out, .. } | Frame::Else { out, .. } => {
                        self.locals.seal_block_preds(out, &mut self.body);
                    }
                    Frame::Loop { out, header, .. } => {
                        self.locals.seal_block_preds(out, &mut self.body);
                        self.locals.seal_block_preds(header, &mut self.body);
                    }
                }
                self.cur_block = Some(frame.out());
                self.push_block_params(frame.results().len());
                self.locals.start_block(frame.out());
            }

            wasmparser::Operator::End => {
                let frame = self.ctrl_stack.pop();
                match &frame {
                    None => {
                        self.emit(Operator::Return)?;
                    }
                    Some(Frame::Block {
                        start_depth,
                        out,
                        ref results,
                        ..
                    })
                    | Some(Frame::Loop {
                        start_depth,
                        out,
                        ref results,
                        ..
                    }) => {
                        // Generate a branch to the out-block with
                        // blockparams for the results.
                        if self.cur_block.is_some() {
                            let result_values = self.pop_n(results.len());
                            self.emit_branch(*out, &result_values[..]);
                        }
                        self.op_stack.truncate(*start_depth);
                        self.locals.finish_block();
                        // Seal the out-block: no more edges will be
                        // added to it. Also, if we're ending a loop,
                        // seal thea header: no more back-edges will
                        // be added to it.
                        self.locals.seal_block_preds(*out, &mut self.body);
                        if let Some(Frame::Loop { header, .. }) = &frame {
                            self.locals.seal_block_preds(*header, &mut self.body);
                        }
                        self.cur_block = Some(*out);
                        self.locals.start_block(*out);
                        self.push_block_params(results.len());
                    }
                    Some(Frame::If {
                        start_depth,
                        out,
                        el,
                        ref param_values,
                        ref results,
                        ..
                    }) => {
                        // Generate a branch to the out-block with
                        // blockparams for the results.
                        let result_values = self.pop_n(results.len());
                        self.emit_branch(*out, &result_values[..]);
                        self.locals.finish_block();
                        self.op_stack.truncate(*start_depth);
                        // No `else`, so we need to generate a trivial
                        // branch in the else-block. If the if-block-type
                        // has results, they must be exactly the params.
                        let else_result_values = param_values;
                        assert_eq!(else_result_values.len(), results.len());
                        let else_result_values = else_result_values
                            .iter()
                            .map(|(_ty, value)| *value)
                            .collect::<Vec<_>>();
                        self.locals.start_block(*el);
                        self.cur_block = Some(*el);
                        self.emit_branch(*out, &else_result_values[..]);
                        self.locals.finish_block();
                        assert_eq!(self.op_stack.len(), *start_depth);
                        self.cur_block = Some(*out);
                        self.locals.seal_block_preds(*out, &mut self.body);
                        self.locals.start_block(*out);
                        self.push_block_params(results.len());
                    }
                    Some(Frame::Else {
                        out,
                        ref results,
                        start_depth,
                        ..
                    }) => {
                        // Generate a branch to the out-block with
                        // blockparams for the results.
                        let result_values = self.pop_n(results.len());
                        self.emit_branch(*out, &result_values[..]);
                        self.locals.finish_block();
                        self.op_stack.truncate(*start_depth);
                        self.cur_block = Some(*out);
                        self.locals.seal_block_preds(*out, &mut self.body);
                        self.locals.start_block(*out);
                        self.push_block_params(results.len());
                    }
                }
            }

            wasmparser::Operator::Block { ty } => {
                let (params, results) = self.block_params_and_results(*ty);
                let out = self.body.add_block();
                self.add_block_params(out, &results[..]);
                let start_depth = self.op_stack.len() - params.len();
                self.ctrl_stack.push(Frame::Block {
                    start_depth,
                    out,
                    params,
                    results,
                });
            }

            wasmparser::Operator::Loop { ty } => {
                let (params, results) = self.block_params_and_results(*ty);
                let header = self.body.add_block();
                self.add_block_params(header, &params[..]);
                let initial_args = self.pop_n(params.len());
                let start_depth = self.op_stack.len();
                self.emit_branch(header, &initial_args[..]);
                self.cur_block = Some(header);
                self.locals.start_block(header);
                self.push_block_params(params.len());
                let out = self.body.add_block();
                self.add_block_params(out, &results[..]);
                self.ctrl_stack.push(Frame::Loop {
                    start_depth,
                    header,
                    out,
                    params,
                    results,
                });
            }

            wasmparser::Operator::If { ty } => {
                let (params, results) = self.block_params_and_results(*ty);
                let if_true = self.body.add_block();
                let if_false = self.body.add_block();
                let join = self.body.add_block();
                self.add_block_params(join, &results[..]);
                let cond = self.pop_1();
                let param_values = self.op_stack[self.op_stack.len() - params.len()..].to_vec();
                let start_depth = self.op_stack.len() - params.len();
                self.ctrl_stack.push(Frame::If {
                    start_depth,
                    out: join,
                    el: if_false,
                    param_values,
                    params,
                    results,
                });
                self.emit_cond_branch(cond, if_true, &[], if_false, &[]);
                self.locals.seal_block_preds(if_true, &mut self.body);
                self.locals.seal_block_preds(if_false, &mut self.body);
                self.cur_block = Some(if_true);
                self.locals.start_block(if_true);
            }

            wasmparser::Operator::Else => {
                if let Frame::If {
                    start_depth,
                    out,
                    el,
                    param_values,
                    params,
                    results,
                } = self.ctrl_stack.pop().unwrap()
                {
                    if self.cur_block.is_some() {
                        let if_results = self.pop_n(results.len());
                        self.emit_branch(out, &if_results[..]);
                    }
                    self.op_stack.truncate(start_depth);
                    self.op_stack.extend(param_values);
                    self.ctrl_stack.push(Frame::Else {
                        start_depth,
                        out,
                        params,
                        results,
                    });
                    self.cur_block = Some(el);
                    self.locals.start_block(el);
                } else {
                    bail!("Else without If on top of frame stack");
                }
            }

            wasmparser::Operator::Br { relative_depth }
            | wasmparser::Operator::BrIf { relative_depth } => {
                let cond = match &op {
                    wasmparser::Operator::Br { .. } => None,
                    wasmparser::Operator::BrIf { .. } => Some(self.pop_1()),
                    _ => unreachable!(),
                };
                // Get the frame we're branching to.
                let frame = self.relative_frame(*relative_depth).clone();
                // Finally, generate the branch itself.
                match cond {
                    None => {
                        // Get the args off the stack unconditionally.
                        let args = self.pop_n(frame.br_args().len());
                        self.emit_branch(frame.br_target(), &args[..]);
                        self.locals.finish_block();
                        self.cur_block = None;
                    }
                    Some(cond) => {
                        let cont = self.body.add_block();
                        // Get the args off the stack but leave for the fallthrough.
                        let args = self.op_stack[self.op_stack.len() - frame.br_args().len()..]
                            .iter()
                            .map(|(_ty, value)| *value)
                            .collect::<Vec<_>>();
                        self.emit_cond_branch(cond, frame.br_target(), &args[..], cont, &[]);
                        self.cur_block = Some(cont);
                        self.locals.seal_block_preds(cont, &mut self.body);
                        self.locals.start_block(cont);
                    }
                }
            }

            wasmparser::Operator::BrTable { table } => {
                // Get the selector index.
                let index = self.pop_1();
                // Get the signature of the default frame; this tells
                // us the signature of all frames (since wasmparser
                // validates the input for us). Pop that many args.
                let default_frame = self.relative_frame(table.default());
                let default_term_target = default_frame.br_target();
                let arg_len = default_frame.br_args().len();
                let args = self.pop_n(arg_len);
                // Generate a branch terminator with the same args for
                // every branch target.
                let mut term_targets = vec![];
                for target in table.targets() {
                    let target = target?;
                    let frame = self.relative_frame(target);
                    assert_eq!(frame.br_args().len(), args.len());
                    let block = frame.br_target();
                    term_targets.push(block);
                }
                self.emit_br_table(index, default_term_target, &term_targets[..], &args[..]);
                self.locals.finish_block();
                self.cur_block = None;
            }

            wasmparser::Operator::Return => {
                let retvals = self.pop_n(self.module.signatures[self.my_sig].returns.len());
                self.emit_ret(&retvals[..]);
                self.locals.finish_block();
                self.cur_block = None;
            }

            _ => bail!("Unsupported operator: {:?}", op),
        }

        Ok(())
    }

    fn add_block_params(&mut self, block: BlockId, tys: &[Type]) {
        log::trace!("add_block_params: block {} tys {:?}", block, tys);
        for &ty in tys {
            self.body.add_blockparam(block, ty);
        }
    }

    fn block_params_and_results(&self, ty: TypeOrFuncType) -> (Vec<Type>, Vec<Type>) {
        match ty {
            TypeOrFuncType::Type(Type::EmptyBlockType) => (vec![], vec![]),
            TypeOrFuncType::Type(ret_ty) => (vec![], vec![ret_ty]),
            TypeOrFuncType::FuncType(sig_idx) => {
                let sig = &self.module.signatures[sig_idx as SignatureId];
                (
                    Vec::from(sig.params.clone()),
                    Vec::from(sig.returns.clone()),
                )
            }
        }
    }

    fn relative_frame(&self, relative_depth: u32) -> &Frame {
        &self.ctrl_stack[self.ctrl_stack.len() - 1 - relative_depth as usize]
    }

    fn emit_branch(&mut self, target: BlockId, args: &[Value]) {
        log::trace!(
            "emit_branch: cur_block {:?} target {} args {:?}",
            self.cur_block,
            target,
            args
        );
        if let Some(block) = self.cur_block {
            let args = args.to_vec();
            self.body.add_edge(block, target);
            let target = BlockTarget {
                block: target,
                args,
            };
            self.body.blocks[block].terminator = Terminator::Br { target };
        }
    }

    fn emit_cond_branch(
        &mut self,
        cond: Value,
        if_true: BlockId,
        if_true_args: &[Value],
        if_false: BlockId,
        if_false_args: &[Value],
    ) {
        log::trace!(
            "emit_cond_branch: cur_block {:?} if_true {} args {:?} if_false {} args {:?}",
            self.cur_block,
            if_true,
            if_true_args,
            if_false,
            if_false_args
        );
        if let Some(block) = self.cur_block {
            let if_true_args = if_true_args.to_vec();
            let if_false_args = if_false_args.to_vec();
            self.body.blocks[block].terminator = Terminator::CondBr {
                cond,
                if_true: BlockTarget {
                    block: if_true,
                    args: if_true_args,
                },
                if_false: BlockTarget {
                    block: if_false,
                    args: if_false_args,
                },
            };
            self.body.add_edge(block, if_true);
            self.body.add_edge(block, if_false);
        }
    }

    fn emit_br_table(
        &mut self,
        index: Value,
        default_target: BlockId,
        indexed_targets: &[BlockId],
        args: &[Value],
    ) {
        log::trace!(
            "emit_br_table: cur_block {:?} index {:?} default {} indexed {:?} args {:?}",
            self.cur_block,
            index,
            default_target,
            indexed_targets,
            args,
        );
        if let Some(block) = self.cur_block {
            let args = args.to_vec();
            let targets = indexed_targets
                .iter()
                .map(|&block| {
                    let args = args.clone();
                    BlockTarget { block, args }
                })
                .collect();

            let default_args = args;
            let default = BlockTarget {
                block: default_target,
                args: default_args,
            };

            for &target in indexed_targets {
                self.body.add_edge(block, target);
            }
            self.body.add_edge(block, default_target);

            self.body.blocks[block].terminator = Terminator::Select {
                value: index,
                targets,
                default,
            };
        }
    }

    fn emit_ret(&mut self, values: &[Value]) {
        if let Some(block) = self.cur_block {
            let values = values.to_vec();
            self.body.blocks[block].terminator = Terminator::Return { values };
        }
    }

    fn push_block_params(&mut self, num_params: usize) {
        log::trace!(
            "push_block_params: cur_block {:?}, {} params",
            self.cur_block,
            num_params
        );
        let block = self.cur_block.unwrap();
        for i in 0..num_params {
            let ty = self.body.blocks[block].params[i];
            let value = self
                .body
                .add_value(ValueDef::BlockParam(block, i), Some(ty));
            log::trace!(" -> push {:?} ty {:?}", value, ty);
            self.op_stack.push((ty, value));
        }
    }

    fn emit(&mut self, op: Operator) -> Result<()> {
        let inputs = op_inputs(
            self.module,
            self.my_sig,
            &self.body.locals[..],
            &self.op_stack[..],
            &op,
        )?;
        let outputs = op_outputs(self.module, &self.body.locals[..], &self.op_stack[..], &op)?;

        log::trace!(
            "emit into block {:?}: op {:?} inputs {:?}",
            self.cur_block,
            op,
            inputs
        );

        if let Some(block) = self.cur_block {
            let n_outputs = outputs.len();

            let mut input_operands = vec![];
            for input in inputs.into_iter().rev() {
                let (stack_top_ty, stack_top) = self.op_stack.pop().unwrap();
                assert_eq!(stack_top_ty, input);
                input_operands.push(stack_top);
            }
            input_operands.reverse();
            log::trace!(" -> operands: {:?}", input_operands);

            let ty = if n_outputs == 1 {
                Some(outputs[0])
            } else {
                None
            };
            let value = self
                .body
                .add_value(ValueDef::Operator(op, input_operands), ty);
            log::trace!(" -> value: {:?} ty {:?}", value, ty);

            if !op_effects(&op).unwrap().is_empty() {
                self.body.blocks[block].insts.push(value);
            }

            if n_outputs == 1 {
                let output_ty = outputs[0];
                self.op_stack.push((output_ty, value));
            } else {
                for (i, output_ty) in outputs.into_iter().enumerate() {
                    let pick = self
                        .body
                        .add_value(ValueDef::PickOutput(value, i), Some(output_ty));
                    self.op_stack.push((output_ty, pick));
                    log::trace!(" -> pick {}: {:?} ty {:?}", i, pick, output_ty);
                }
            }
        } else {
            let _ = self.pop_n(inputs.len());
            for ty in outputs {
                self.op_stack.push((ty, Value::undef()));
            }
        }

        Ok(())
    }
}
