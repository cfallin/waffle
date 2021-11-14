//! IR-to-Wasm transform.

use crate::ir::*;
use fxhash::{FxHashMap, FxHashSet};

pub fn treeify_function(func: &mut FunctionBody) -> FxHashSet<(BlockId, InstId)> {
    // First, count uses of all values.
    let mut uses: FxHashMap<(BlockId, InstId, usize), usize> = FxHashMap::default();
    for block in &func.blocks {
        for inst in &block.insts {
            for input in &inst.inputs {
                match input {
                    &Operand::Value(value_id) => {
                        if let ValueKind::Inst(src_block, src_inst, idx) =
                            &func.values[value_id].kind
                        {
                            *uses.entry((*src_block, *src_inst, *idx)).or_insert(0) += 1;
                        }
                    }
                    _ => {}
                }
            }
        }

        for arg in block.terminator.args() {
            match arg {
                Operand::Value(value_id) => {
                    if let ValueKind::Inst(src_block, src_inst, idx) = &func.values[value_id].kind {
                        *uses.entry((*src_block, *src_inst, *idx)).or_insert(0) += 1;
                    }
                }
                _ => {}
            }
        }
    }

    // Next, treeify all insts with only one use.
    let mut single_use_insts: FxHashSet<(BlockId, InstId)> = FxHashSet::default();
    for (block_idx, block) in func.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            let all_one_use = (0..inst.outputs.len()).all(|output| {
                uses.get(&(block_idx, inst_idx, output))
                    .cloned()
                    .unwrap_or(0)
                    <= 1
            });
            if all_one_use {
                single_use_insts.insert((block_idx, inst_idx));
            }
        }
    }
    
    single_use_insts
}
