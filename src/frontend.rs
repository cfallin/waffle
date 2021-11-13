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
                if let ImportSectionEntryType::Function(sig_idx) = reader.read()?.ty {
                    module.funcs.push(FuncDecl::Import(sig_idx as SignatureId));
                    *next_func += 1;
                }
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
    let mut locals = body.get_locals_reader()?;
    for _ in 0..locals.get_count() {
        let (count, ty) = locals.read()?;
        for _ in 0..count {
            ret.locals.push(ty);
        }
    }

    let mut builder = FunctionBodyBuilder::new(module, my_sig, &mut ret);
    let ops = body.get_operators_reader()?;
    for op in ops.into_iter() {
        let op = op?;
        builder.handle_op(op)?;
    }

    Ok(ret)
}

#[derive(Debug)]
struct FunctionBodyBuilder<'a, 'b> {
    module: &'b Module<'a>,
    my_sig: SignatureId,
    body: &'b mut FunctionBody<'a>,
    cur_block: BlockId,
    ctrl_stack: Vec<Frame>,
    op_stack: Vec<ValueId>,
}

#[derive(Debug)]
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
        param_values: Vec<ValueId>,
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
}

impl<'a, 'b> FunctionBodyBuilder<'a, 'b> {
    fn new(module: &'b Module<'a>, my_sig: SignatureId, body: &'b mut FunctionBody<'a>) -> Self {
        body.blocks.push(Block::default());
        Self {
            module,
            my_sig,
            body,
            ctrl_stack: vec![],
            op_stack: vec![],
            cur_block: 0,
        }
    }

    fn add_block_params(&mut self, block: BlockId, tys: &[Type]) {
        self.body.blocks[block].params.extend_from_slice(tys);
    }

    fn handle_op(&mut self, op: Operator<'a>) -> Result<()> {
        match &op {
            Operator::Unreachable
            | Operator::Call { .. }
            | Operator::LocalGet { .. }
            | Operator::LocalSet { .. }
            | Operator::LocalTee { .. } => self.emit(op.clone())?,

            Operator::End => match self.ctrl_stack.pop() {
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
                    let result_values = self.op_stack.split_off(results.len());
                    self.emit_branch(out, &result_values[..]);
                    assert_eq!(self.op_stack.len(), start_depth);
                    self.cur_block = out;
                    self.push_block_params(&results[..]);
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
                    let result_values = self.op_stack.split_off(results.len());
                    self.emit_branch(out, &result_values[..]);
                    // No `else`, so we need to generate a trivial
                    // branch in the else-block. If the if-block-type
                    // has results, they must be exactly the params.
                    let else_result_values = param_values;
                    assert_eq!(else_result_values.len(), results.len());
                    self.emit_branch(el, &else_result_values[..]);
                    assert_eq!(self.op_stack.len(), start_depth);
                    self.cur_block = out;
                    self.push_block_params(&results[..]);
                }
                Some(Frame::Else {
                    out,
                    results,
                    start_depth,
                    ..
                }) => {
                    // Generate a branch to the out-block with
                    // blockparams for the results.
                    let result_values = self.op_stack.split_off(results.len());
                    assert_eq!(self.op_stack.len(), start_depth);
                    self.emit_branch(out, &result_values[..]);
                    self.cur_block = out;
                    self.push_block_params(&results[..]);
                }
            },

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
                let initial_args = self.op_stack.split_off(params.len());
                let start_depth = self.op_stack.len();
                self.emit_branch(header, &initial_args[..]);
                self.cur_block = header;
                self.push_block_params(&params[..]);
                let out = self.create_block();
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
                let cond = self.op_stack.pop().unwrap();
                let param_values = self.op_stack[self.op_stack.len() - params.len()..].to_vec();
                let start_depth = self.op_stack.len();
                self.ctrl_stack.push(Frame::If {
                    start_depth,
                    out: join,
                    el: if_false,
                    param_values,
                    params,
                    results,
                });
                self.cur_block = if_true;
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
                    let if_results = self.op_stack.split_off(results.len());
                    self.emit_branch(out, &if_results[..]);
                    self.op_stack.extend(param_values);
                    self.ctrl_stack.push(Frame::Else {
                        start_depth,
                        out,
                        params,
                        results,
                    });
                    self.cur_block = el;
                } else {
                    bail!("Else without If on top of frame stack");
                }
            }

            Operator::Br { relative_depth } | Operator::BrIf { relative_depth } => {
                let cond = match &op {
                    Operator::Br { .. } => None,
                    Operator::BrIf { .. } => Some(self.op_stack.pop().unwrap()),
                    _ => unreachable!(),
                };
                // Pop skipped-over frames.
                let _ = self.ctrl_stack.split_off(*relative_depth as usize);
                // Get the frame we're branching to.
                let frame = self.ctrl_stack.pop().unwrap();
                // Get the args off the stack.
                let args = self.op_stack.split_off(frame.br_args().len());
                // Truncate the result stack down to the expected height.
                self.op_stack.truncate(frame.start_depth());
                // Finally, generate the branch itself.
                match cond {
                    None => {
                        self.emit_branch(frame.br_target(), &args[..]);
                    }
                    Some(cond) => {
                        let cont = self.create_block();
                        self.emit_cond_branch(cond, frame.br_target(), &args[..], cont, &[]);
                        self.cur_block = cont;
                    }
                }
            }

            Operator::BrTable { .. } => {}

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

    fn emit_branch(&mut self, target: BlockId, args: &[ValueId]) {
        let block = self.cur_block;
        let args = args.iter().map(|&val| Operand::Value(val)).collect();
        let target = BlockTarget {
            block: target,
            args,
        };
        self.body.blocks[block].terminator = Terminator::Br { target };
    }

    fn emit_cond_branch(
        &mut self,
        cond: ValueId,
        if_true: BlockId,
        if_true_args: &[ValueId],
        if_false: BlockId,
        if_false_args: &[ValueId],
    ) {
        let block = self.cur_block;
        let if_true_args = if_true_args
            .iter()
            .map(|&val| Operand::Value(val))
            .collect();
        let if_false_args = if_false_args
            .iter()
            .map(|&val| Operand::Value(val))
            .collect();
        self.body.blocks[block].terminator = Terminator::CondBr {
            cond: Operand::Value(cond),
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

    fn push_block_params(&mut self, tys: &[Type]) {
        assert_eq!(tys, self.body.blocks[self.cur_block].params);
        for (i, &ty) in tys.iter().enumerate() {
            let value_id = self.body.values.len() as ValueId;
            self.body.values.push(ValueDef {
                kind: ValueKind::BlockParam(self.cur_block, i),
                ty,
            });
            self.op_stack.push(value_id);
        }
    }

    fn emit(&mut self, op: Operator<'a>) -> Result<()> {
        let block = self.cur_block;
        let inst = self.body.blocks[block].insts.len() as InstId;

        let mut inputs = vec![];
        for input in op_inputs(self.module, self.my_sig, &self.body.locals[..], &op)?
            .into_iter()
            .rev()
        {
            let stack_top = self.op_stack.pop().unwrap();
            assert_eq!(self.body.values[stack_top].ty, input);
            inputs.push(Operand::Value(stack_top));
        }
        inputs.reverse();

        let mut outputs = vec![];
        for output in op_outputs(self.module, &self.body.locals[..], &op)?.into_iter() {
            let val = self.body.values.len() as ValueId;
            outputs.push(val);
            self.body.values.push(ValueDef {
                kind: ValueKind::Inst(block, inst),
                ty: output,
            });
            self.op_stack.push(val);
        }

        self.body.blocks[self.cur_block].insts.push(Inst {
            operator: op,
            outputs,
            inputs,
        });

        Ok(())
    }
}
