//! Frontend: convert Wasm to IR.

#![allow(dead_code)]

use crate::ir::*;
use crate::op_traits::{op_inputs, op_outputs};
use anyhow::{bail, Result};
use log::trace;
use wasmparser::{
    ImportSectionEntryType, Operator, Parser, Payload, Type, TypeDef, TypeOrFuncType,
};

pub fn wasm_to_ir(bytes: &[u8]) -> Result<Module<'_>> {
    let mut module = Module::default();
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

fn parse_body<'a, 'b>(
    module: &'b Module<'a>,
    my_sig: SignatureId,
    body: wasmparser::FunctionBody<'a>,
) -> Result<FunctionBody<'a>> {
    let mut ret: FunctionBody<'a> = FunctionBody::default();

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

    trace!(
        "Parsing function body: locals = {:?} sig = {:?}",
        ret.locals,
        module.signatures[my_sig]
    );

    let mut builder = FunctionBodyBuilder::new(module, my_sig, &mut ret);
    let ops = body.get_operators_reader()?;
    for op in ops.into_iter() {
        let op = op?;
        builder.handle_op(op)?;
    }

    if builder.cur_block.is_some() {
        builder.handle_op(Operator::Return)?;
    }

    trace!("Final function body:{:?}", ret);

    Ok(ret)
}

#[derive(Debug)]
struct FunctionBodyBuilder<'a, 'b> {
    module: &'b Module<'a>,
    my_sig: SignatureId,
    body: &'b mut FunctionBody<'a>,
    cur_block: Option<BlockId>,
    ctrl_stack: Vec<Frame>,
    op_stack: Vec<(Type, ValueId)>,
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
        param_values: Vec<(Type, ValueId)>,
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
}

impl<'a, 'b> FunctionBodyBuilder<'a, 'b> {
    fn new(module: &'b Module<'a>, my_sig: SignatureId, body: &'b mut FunctionBody<'a>) -> Self {
        body.blocks.push(Block::default());
        let mut ret = Self {
            module,
            my_sig,
            body,
            ctrl_stack: vec![],
            op_stack: vec![],
            cur_block: Some(0),
        };

        // Push initial implicit Block.
        let results = module.signatures[my_sig].returns.to_vec();
        let out = ret.create_block();
        ret.add_block_params(out, &results[..]);
        ret.ctrl_stack.push(Frame::Block {
            start_depth: 0,
            out,
            params: vec![],
            results,
        });
        ret
    }

    fn add_block_params(&mut self, block: BlockId, tys: &[Type]) {
        self.body.blocks[block].params.extend_from_slice(tys);
    }

    fn pop_n(&mut self, n: usize) -> Vec<ValueId> {
        let new_top = self.op_stack.len() - n;
        let ret = self.op_stack[new_top..]
            .iter()
            .map(|(_ty, value)| *value)
            .collect::<Vec<_>>();
        self.op_stack.truncate(new_top);
        ret
    }

    fn pop_1(&mut self) -> ValueId {
        self.op_stack.pop().unwrap().1
    }

    fn handle_op(&mut self, op: Operator<'a>) -> Result<()> {
        trace!("handle_op: {:?}", op);
        trace!("op_stack = {:?}", self.op_stack);
        trace!("ctrl_stack = {:?}", self.ctrl_stack);
        match &op {
            Operator::Unreachable => {
                if let Some(block) = self.cur_block {
                    self.body.blocks[block].terminator = Terminator::None;
                }
                self.cur_block = None;
            }

            Operator::LocalGet { .. }
            | Operator::LocalSet { .. }
            | Operator::LocalTee { .. }
            | Operator::Call { .. }
            | Operator::CallIndirect { .. }
            | Operator::Select
            | Operator::TypedSelect { .. }
            | Operator::GlobalGet { .. }
            | Operator::GlobalSet { .. }
            | Operator::I32Load { .. }
            | Operator::I64Load { .. }
            | Operator::F32Load { .. }
            | Operator::F64Load { .. }
            | Operator::I32Load8S { .. }
            | Operator::I32Load8U { .. }
            | Operator::I32Load16S { .. }
            | Operator::I32Load16U { .. }
            | Operator::I64Load8S { .. }
            | Operator::I64Load8U { .. }
            | Operator::I64Load16S { .. }
            | Operator::I64Load16U { .. }
            | Operator::I64Load32S { .. }
            | Operator::I64Load32U { .. }
            | Operator::I32Store { .. }
            | Operator::I64Store { .. }
            | Operator::F32Store { .. }
            | Operator::F64Store { .. }
            | Operator::I32Store8 { .. }
            | Operator::I32Store16 { .. }
            | Operator::I64Store8 { .. }
            | Operator::I64Store16 { .. }
            | Operator::I64Store32 { .. }
            | Operator::MemorySize { .. }
            | Operator::MemoryGrow { .. }
            | Operator::I32Const { .. }
            | Operator::I64Const { .. }
            | Operator::F32Const { .. }
            | Operator::F64Const { .. }
            | Operator::I32Eqz
            | Operator::I32Eq
            | Operator::I32Ne
            | Operator::I32LtS
            | Operator::I32LtU
            | Operator::I32GtS
            | Operator::I32GtU
            | Operator::I32LeS
            | Operator::I32LeU
            | Operator::I32GeS
            | Operator::I32GeU
            | Operator::I64Eqz
            | Operator::I64Eq
            | Operator::I64Ne
            | Operator::I64LtS
            | Operator::I64LtU
            | Operator::I64GtU
            | Operator::I64GtS
            | Operator::I64LeS
            | Operator::I64LeU
            | Operator::I64GeS
            | Operator::I64GeU
            | Operator::F32Eq
            | Operator::F32Ne
            | Operator::F32Lt
            | Operator::F32Gt
            | Operator::F32Le
            | Operator::F32Ge
            | Operator::F64Eq
            | Operator::F64Ne
            | Operator::F64Lt
            | Operator::F64Gt
            | Operator::F64Le
            | Operator::F64Ge
            | Operator::I32Clz
            | Operator::I32Ctz
            | Operator::I32Popcnt
            | Operator::I32Add
            | Operator::I32Sub
            | Operator::I32Mul
            | Operator::I32DivS
            | Operator::I32DivU
            | Operator::I32RemS
            | Operator::I32RemU
            | Operator::I32And
            | Operator::I32Or
            | Operator::I32Xor
            | Operator::I32Shl
            | Operator::I32ShrS
            | Operator::I32ShrU
            | Operator::I32Rotl
            | Operator::I32Rotr
            | Operator::I64Clz
            | Operator::I64Ctz
            | Operator::I64Popcnt
            | Operator::I64Add
            | Operator::I64Sub
            | Operator::I64Mul
            | Operator::I64DivS
            | Operator::I64DivU
            | Operator::I64RemS
            | Operator::I64RemU
            | Operator::I64And
            | Operator::I64Or
            | Operator::I64Xor
            | Operator::I64Shl
            | Operator::I64ShrS
            | Operator::I64ShrU
            | Operator::I64Rotl
            | Operator::I64Rotr
            | Operator::F32Abs
            | Operator::F32Neg
            | Operator::F32Ceil
            | Operator::F32Floor
            | Operator::F32Trunc
            | Operator::F32Nearest
            | Operator::F32Sqrt
            | Operator::F32Add
            | Operator::F32Sub
            | Operator::F32Mul
            | Operator::F32Div
            | Operator::F32Min
            | Operator::F32Max
            | Operator::F32Copysign
            | Operator::F64Abs
            | Operator::F64Neg
            | Operator::F64Ceil
            | Operator::F64Floor
            | Operator::F64Trunc
            | Operator::F64Nearest
            | Operator::F64Sqrt
            | Operator::F64Add
            | Operator::F64Sub
            | Operator::F64Mul
            | Operator::F64Div
            | Operator::F64Min
            | Operator::F64Max
            | Operator::F64Copysign
            | Operator::I32WrapI64
            | Operator::I32TruncF32S
            | Operator::I32TruncF32U
            | Operator::I32TruncF64S
            | Operator::I32TruncF64U
            | Operator::I64ExtendI32S
            | Operator::I64ExtendI32U
            | Operator::I64TruncF32S
            | Operator::I64TruncF32U
            | Operator::I64TruncF64S
            | Operator::I64TruncF64U
            | Operator::F32ConvertI32S
            | Operator::F32ConvertI32U
            | Operator::F32ConvertI64S
            | Operator::F32ConvertI64U
            | Operator::F32DemoteF64
            | Operator::F64ConvertI32S
            | Operator::F64ConvertI32U
            | Operator::F64ConvertI64S
            | Operator::F64ConvertI64U
            | Operator::F64PromoteF32
            | Operator::I32Extend8S
            | Operator::I32Extend16S
            | Operator::I64Extend8S
            | Operator::I64Extend16S
            | Operator::I64Extend32S
            | Operator::I32TruncSatF32S
            | Operator::I32TruncSatF32U
            | Operator::I32TruncSatF64S
            | Operator::I32TruncSatF64U
            | Operator::I64TruncSatF32S
            | Operator::I64TruncSatF32U
            | Operator::I64TruncSatF64S
            | Operator::I64TruncSatF64U
            | Operator::F32ReinterpretI32
            | Operator::F64ReinterpretI64
            | Operator::I32ReinterpretF32
            | Operator::I64ReinterpretF64
            | Operator::TableGet { .. }
            | Operator::TableSet { .. }
            | Operator::TableGrow { .. }
            | Operator::TableSize { .. } => self.emit(op.clone())?,

            Operator::Nop => {}

            Operator::Drop => {
                let _ = self.pop_1();
            }

            Operator::End if self.cur_block.is_none() => {
                let frame = self.ctrl_stack.pop().unwrap();
                self.op_stack.truncate(frame.start_depth());
                self.cur_block = Some(frame.out());
                self.push_block_params();
            }

            Operator::End => {
                let frame = self.ctrl_stack.pop();
                match frame {
                    None => {
                        self.emit(Operator::Return)?;
                    }
                    Some(Frame::Block {
                        start_depth,
                        out,
                        results,
                        ..
                    })
                    | Some(Frame::Loop {
                        start_depth,
                        out,
                        results,
                        ..
                    }) => {
                        // Generate a branch to the out-block with
                        // blockparams for the results.
                        if self.cur_block.is_some() {
                            let result_values = self.pop_n(results.len());
                            self.emit_branch(out, &result_values[..]);
                        }
                        self.op_stack.truncate(start_depth);
                        self.cur_block = Some(out);
                        self.push_block_params();
                    }
                    Some(Frame::If {
                        start_depth,
                        out,
                        el,
                        param_values,
                        results,
                        ..
                    }) => {
                        // Generate a branch to the out-block with
                        // blockparams for the results.
                        if self.cur_block.is_some() {
                            let result_values = self.pop_n(results.len());
                            self.emit_branch(out, &result_values[..]);
                        }
                        self.op_stack.truncate(start_depth);
                        // No `else`, so we need to generate a trivial
                        // branch in the else-block. If the if-block-type
                        // has results, they must be exactly the params.
                        let else_result_values = param_values;
                        assert_eq!(else_result_values.len(), results.len());
                        let else_result_values = else_result_values
                            .iter()
                            .map(|(_ty, value)| *value)
                            .collect::<Vec<_>>();
                        self.emit_branch(el, &else_result_values[..]);
                        assert_eq!(self.op_stack.len(), start_depth);
                        self.cur_block = Some(out);
                        self.push_block_params();
                    }
                    Some(Frame::Else {
                        out,
                        results,
                        start_depth,
                        ..
                    }) => {
                        // Generate a branch to the out-block with
                        // blockparams for the results.
                        if self.cur_block.is_some() {
                            let result_values = self.pop_n(results.len());
                            self.emit_branch(out, &result_values[..]);
                        }
                        self.op_stack.truncate(start_depth);
                        self.cur_block = Some(out);
                        self.push_block_params();
                    }
                }
            }

            Operator::Block { ty } => {
                let (params, results) = self.block_params_and_results(*ty);
                let out = self.create_block();
                self.add_block_params(out, &results[..]);
                let start_depth = self.op_stack.len() - params.len();
                self.ctrl_stack.push(Frame::Block {
                    start_depth,
                    out,
                    params,
                    results,
                });
            }

            Operator::Loop { ty } => {
                let (params, results) = self.block_params_and_results(*ty);
                let header = self.create_block();
                self.add_block_params(header, &params[..]);
                let initial_args = self.pop_n(params.len());
                let start_depth = self.op_stack.len();
                self.emit_branch(header, &initial_args[..]);
                self.cur_block = Some(header);
                self.push_block_params();
                let out = self.create_block();
                self.add_block_params(out, &results[..]);
                self.ctrl_stack.push(Frame::Loop {
                    start_depth,
                    header,
                    out,
                    params,
                    results,
                });
            }

            Operator::If { ty } => {
                let (params, results) = self.block_params_and_results(*ty);
                let if_true = self.create_block();
                let if_false = self.create_block();
                let join = self.create_block();
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
                self.cur_block = Some(if_true);
                self.emit_cond_branch(cond, if_true, &[], if_false, &[]);
            }

            Operator::Else => {
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
                } else {
                    bail!("Else without If on top of frame stack");
                }
            }

            Operator::Br { relative_depth } | Operator::BrIf { relative_depth } => {
                let cond = match &op {
                    Operator::Br { .. } => None,
                    Operator::BrIf { .. } => Some(self.pop_1()),
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
                        self.cur_block = None;
                    }
                    Some(cond) => {
                        let cont = self.create_block();
                        // Get the args off the stack but leave for the fallthrough.
                        let args = self.op_stack[self.op_stack.len() - frame.br_args().len()..]
                            .iter()
                            .map(|(_ty, value)| *value)
                            .collect::<Vec<_>>();
                        self.emit_cond_branch(cond, frame.br_target(), &args[..], cont, &[]);
                        self.cur_block = Some(cont);
                    }
                }
            }

            Operator::BrTable { table } => {
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
                self.cur_block = None;
            }

            Operator::Return => {
                let retvals = self.pop_n(self.module.signatures[self.my_sig].returns.len());
                self.emit_ret(&retvals[..]);
                self.cur_block = None;
            }

            _ => bail!("Unsupported operator: {:?}", op),
        }

        Ok(())
    }

    fn create_block(&mut self) -> BlockId {
        let id = self.body.blocks.len() as BlockId;
        self.body.blocks.push(Block::default());
        id
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

    fn emit_branch(&mut self, target: BlockId, args: &[ValueId]) {
        if let Some(block) = self.cur_block {
            let args = args.iter().map(|&val| Operand::value(val)).collect();
            let target = BlockTarget {
                block: target,
                args,
            };
            self.body.blocks[block].terminator = Terminator::Br { target };
        }
    }

    fn emit_cond_branch(
        &mut self,
        cond: ValueId,
        if_true: BlockId,
        if_true_args: &[ValueId],
        if_false: BlockId,
        if_false_args: &[ValueId],
    ) {
        if let Some(block) = self.cur_block {
            let if_true_args = if_true_args
                .iter()
                .map(|&val| Operand::value(val))
                .collect();
            let if_false_args = if_false_args
                .iter()
                .map(|&val| Operand::value(val))
                .collect();
            self.body.blocks[block].terminator = Terminator::CondBr {
                cond: Operand::value(cond),
                if_true: BlockTarget {
                    block: if_true,
                    args: if_true_args,
                },
                if_false: BlockTarget {
                    block: if_false,
                    args: if_false_args,
                },
            };
        }
    }

    fn emit_br_table(
        &mut self,
        index: ValueId,
        default_target: BlockId,
        indexed_targets: &[BlockId],
        args: &[ValueId],
    ) {
        if let Some(block) = self.cur_block {
            let args: Vec<Operand<'a>> = args.iter().map(|&arg| Operand::value(arg)).collect();
            let targets = indexed_targets
                .iter()
                .map(|&block| BlockTarget {
                    block,
                    args: args.clone(),
                })
                .collect();
            let default = BlockTarget {
                block: default_target,
                args: args.clone(),
            };
            self.body.blocks[block].terminator = Terminator::Select {
                value: Operand::value(index),
                targets,
                default,
            };
        }
    }

    fn emit_ret(&mut self, vals: &[ValueId]) {
        if let Some(block) = self.cur_block {
            let values = vals.iter().map(|&value| Operand::value(value)).collect();
            self.body.blocks[block].terminator = Terminator::Return { values };
        }
    }

    fn push_block_params(&mut self) {
        let tys = &self.body.blocks[self.cur_block.unwrap()].params[..];

        for (i, &ty) in tys.iter().enumerate() {
            let value_id = self.body.values.len() as ValueId;
            self.body.values.push(ValueDef {
                kind: ValueKind::BlockParam(self.cur_block.unwrap(), i),
                ty,
            });
            self.op_stack.push((ty, value_id));
        }
    }

    fn emit(&mut self, op: Operator<'a>) -> Result<()> {
        let inputs = op_inputs(
            self.module,
            self.my_sig,
            &self.body.locals[..],
            &self.op_stack[..],
            &op,
        )?;
        let outputs = op_outputs(self.module, &self.body.locals[..], &self.op_stack[..], &op)?;

        if let Some(block) = self.cur_block {
            let inst = self.body.blocks[block].insts.len() as InstId;

            let mut input_operands = vec![];
            for input in inputs.into_iter().rev() {
                let (stack_top_ty, stack_top) = self.op_stack.pop().unwrap();
                assert_eq!(stack_top_ty, input);
                input_operands.push(Operand::value(stack_top));
            }
            input_operands.reverse();

            let mut output_operands = vec![];
            for output_ty in outputs.into_iter() {
                let val = self.body.values.len() as ValueId;
                output_operands.push(val);
                self.body.values.push(ValueDef {
                    kind: ValueKind::Inst(block, inst),
                    ty: output_ty,
                });
                self.op_stack.push((output_ty, val));
            }

            self.body.blocks[block].insts.push(Inst {
                operator: op,
                outputs: output_operands,
                inputs: input_operands,
            });
        } else {
            let _ = self.pop_n(inputs.len());
            for ty in outputs {
                self.op_stack.push((ty, NO_VALUE));
            }
        }

        Ok(())
    }
}
