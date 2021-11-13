//! Frontend: convert Wasm to IR.

#![allow(dead_code)]

use crate::ir::*;
use crate::op_traits::{op_inputs, op_outputs};
use anyhow::anyhow;
use anyhow::{bail, Result};
use log::trace;
use std::collections::VecDeque;
use wasmparser::{FuncType, ImportSectionEntryType, Operator, Parser, Payload, Type, TypeDef};

pub fn wasm_to_ir(bytes: &[u8]) -> Result<Module> {
    let mut module = Module::default();
    let parser = Parser::new(0);
    let mut sigs = VecDeque::new();
    for payload in parser.parse_all(bytes) {
        let payload = payload?;
        handle_payload(&mut module, payload, &mut sigs)?;
    }

    Ok(module)
}

fn handle_payload<'a>(
    module: &mut Module,
    payload: Payload<'a>,
    func_sigs: &mut VecDeque<SignatureId>,
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
                    }
                    _ => {}
                }
            }
        }
        Payload::FunctionSection(mut reader) => {
            for _ in 0..reader.get_count() {
                func_sigs.push_back(reader.read()? as SignatureId);
            }
        }
        Payload::CodeSectionEntry(body) => {
            let sig = func_sigs
                .pop_front()
                .ok_or_else(|| anyhow!("mismatched func section and code section sizes"))?;
            let body = parse_body(&module, body)?;
            module.funcs.push(FuncDecl::Body(sig as SignatureId, body));
        }
        _ => {}
    }

    Ok(())
}

fn parse_body(module: &Module, body: wasmparser::FunctionBody) -> Result<FunctionBody> {
    let mut ret = FunctionBody::default();
    let mut locals = body.get_locals_reader()?;
    for _ in 0..locals.get_count() {
        let (count, ty) = locals.read()?;
        for _ in 0..count {
            ret.locals.push(ty);
        }
    }

    let mut builder = FunctionBodyBuilder::new(&module.signatures[..], &mut ret);
    let ops = body.get_operators_reader()?;
    for op in ops.into_iter() {
        let op = op?;
        builder.handle_op(op)?;
    }

    Ok(ret)
}

#[derive(Debug)]
struct FunctionBodyBuilder<'a> {
    signatures: &'a [FuncType],
    body: &'a mut FunctionBody,
    cur_block: BlockId,
    ctrl_stack: Vec<Frame>,
    op_stack: Vec<ValueId>,
}

#[derive(Debug)]
enum Frame {
    Block {
        out: Block,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    Loop {
        top: Block,
        out: Block,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    If {
        out: Block,
        el: Block,
        params: Vec<Type>,
        results: Vec<Type>,
    },
    Else {
        out: Block,
        params: Vec<Type>,
        results: Vec<Type>,
    },
}

impl<'a> FunctionBodyBuilder<'a> {
    fn new(signatures: &'a [FuncType], body: &'a mut FunctionBody) -> Self {
        body.blocks.push(Block::default());
        Self {
            signatures,
            body,
            ctrl_stack: vec![],
            op_stack: vec![],
            cur_block: 0,
        }
    }

    fn handle_op(&mut self, op: Operator<'_>) -> Result<()> {
        match op {
            Operator::Unreachable => self.emit(Operator::Unreachable, vec![], vec![])?,
            _ => bail!("Unsupported operator: {:?}", op),
        }

        Ok(())
    }

    fn emit(
        &mut self,
        op: Operator<'static>,
        outputs: Vec<ValueId>,
        inputs: Vec<Operand>,
    ) -> Result<()> {
        let block = self.cur_block;
        let inst = self.body.blocks[block].insts.len() as InstId;

        for input in op_inputs(self.signatures, &op)?.into_iter().rev() {
            assert_eq!(self.body.values[self.op_stack.pop().unwrap()].ty, input);
        }
        for output in op_outputs(self.signatures, &op)?.into_iter() {
            let val = self.body.values.len() as ValueId;
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
