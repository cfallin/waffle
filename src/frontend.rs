//! Frontend: convert Wasm to IR.

use crate::ir::*;
use anyhow::{bail, Result};
use log::debug;
use wasmparser::{Chunk, Parser, Payload, TypeDef};

pub fn wasm_to_ir(bytes: &[u8]) -> Result<Module> {
    let mut module = Module::default();
    let mut parser = Parser::new(0);
    for chunk in parser.parse(bytes, /* eof = */ true) {
        match chunk {
            Chunk::NeedMoreData(_) => bail!("Unexpected EOF in Wasm"),
            Chunk::Parsed { payload, .. } => handle_payload(&mut module, payload)?,
        }
    }

    Ok(module)
}

fn handle_payload<'a>(module: &mut Module, payload: Payload<'a>) -> Result<()> {
    debug!("Wasm parser item: {:?}", payload);
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
        _ => {}
    }

    Ok(())
}
