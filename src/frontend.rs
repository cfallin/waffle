//! Frontend: convert Wasm to IR.

use crate::ir::*;
use anyhow::Result;
use log::trace;
use wasmparser::{ImportSectionEntryType, Parser, Payload, TypeDef};

pub fn wasm_to_ir(bytes: &[u8]) -> Result<Module> {
    let mut module = Module::default();
    let parser = Parser::new(0);
    for payload in parser.parse_all(bytes) {
        let payload = payload?;
        handle_payload(&mut module, payload)?;
    }

    Ok(module)
}

fn handle_payload<'a>(module: &mut Module, payload: Payload<'a>) -> Result<()> {
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
        _ => {}
    }

    Ok(())
}
