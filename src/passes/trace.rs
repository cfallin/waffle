//! Trace-insertion pass.

use crate::entity::EntityRef;
use crate::ir::*;

pub fn run(body: &mut FunctionBody) {
    for (block, data) in body.blocks.entries_mut() {
        let value = ValueDef::Trace(
            block.index(),
            data.params
                .iter()
                .map(|&(_, param)| param)
                .collect::<Vec<_>>(),
        );
        let value = body.values.push(value);
        data.insts.insert(0, value);
    }
}
