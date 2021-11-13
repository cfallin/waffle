//! Frontend: convert Wasm to IR.

#![allow(dead_code)]

use crate::ir::*;
use crate::op_traits::{op_inputs, op_outputs};
use anyhow::{bail, Result};
use log::trace;
use wasmparser::{ImportSectionEntryType, Operator, Parser, Payload, Type, TypeDef};

pub fn wasm_to_ir<'a>(bytes: &'a [u8]) -> Result<Module<'a>> {
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
                match ty {
                    TypeDef::Func(fty) => {
                        module.signatures.push(fty);
                    }
                    _ => {}
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
                    _ => {}
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
            let body = parse_body(&module, my_sig, body)?;

            match &mut module.funcs[func_idx] {
                &mut FuncDecl::Body(_, ref mut existing_body) => {
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

    let mut builder = FunctionBodyBuilder::new(&module, my_sig, &mut ret);
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
        out: BlockId,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    Loop {
        top: BlockId,
        out: BlockId,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    If {
        out: BlockId,
        el: BlockId,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    Else {
        out: BlockId,
        params: Vec<Type>,
        results: Vec<Type>,
    },
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

    fn handle_op(&mut self, op: Operator<'a>) -> Result<()> {
        match op {
            Operator::Unreachable
            | Operator::Call { .. }
            | Operator::LocalGet { .. }
            | Operator::LocalSet { .. }
            | Operator::LocalTee { .. } => self.emit(op.clone())?,

            Operator::End => {
                if self.ctrl_stack.is_empty() {
                    self.emit(Operator::Return)?;
                } else {
                    bail!("Unsupported End");
                }
            }

            _ => bail!("Unsupported operator: {:?}", op),
        }

        Ok(())
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
