//! Resolve all aliases.

use crate::{FunctionBody, ValueDef};

pub fn run(body: &mut FunctionBody) {
    for value in body.values.iter() {
        let mut value_def = std::mem::take(&mut body.values[value]);
        match &mut value_def {
            ValueDef::Operator(_op, args, _tys) => {
                for arg in args {
                    *arg = body.resolve_alias(*arg);
                }
            }
            ValueDef::PickOutput(val, _idx, _ty) => {
                *val = body.resolve_alias(*val);
            }
            ValueDef::Alias(val) => {
                *val = body.resolve_alias(*val);
            }
            _ => {}
        }
        body.values[value] = value_def;
    }
    let mut blocks = std::mem::take(&mut body.blocks);
    for block in blocks.values_mut() {
        block.terminator.update_targets(|target| {
            for arg in &mut target.args {
                *arg = body.resolve_alias(*arg);
            }
        });
    }
    body.blocks = blocks;
}
