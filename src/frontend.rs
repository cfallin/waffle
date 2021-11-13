//! Frontend: convert Wasm to IR.

use crate::ir::*;
use anyhow::anyhow;
use anyhow::Result;
use log::trace;
use std::collections::VecDeque;
use wasmparser::{ImportSectionEntryType, Operator, Parser, Payload, TypeDef};

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
            let body = parse_body(body)?;
            module.funcs.push(FuncDecl::Body(sig as SignatureId, body));
        }
        _ => {}
    }

    Ok(())
}

fn parse_body(body: wasmparser::FunctionBody) -> Result<FunctionBody> {
    let mut ret = FunctionBody::default();
    let mut locals = body.get_locals_reader()?;
    for _ in 0..locals.get_count() {
        let (count, ty) = locals.read()?;
        for _ in 0..count {
            ret.locals.push(ty);
        }
    }

    let mut builder = FunctionBodyBuilder::new(&mut ret);
    let ops = body.get_operators_reader()?;
    for op in ops.into_iter() {
        let op = op?;
        builder.handle_op(op)?;
    }

    Ok(ret)
}

#[derive(Debug)]
struct FunctionBodyBuilder<'a> {
    body: &'a mut FunctionBody,
}

impl<'a> FunctionBodyBuilder<'a> {
    fn new(body: &'a mut FunctionBody) -> Self {
        Self { body }
    }

    fn handle_op(&mut self, op: Operator<'_>) -> Result<()> {
        match op {
            _ => {}
        }

        Ok(())
    }
}
