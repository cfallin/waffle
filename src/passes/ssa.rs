//! SSA validation.

use crate::cfg::CFGInfo;
use crate::entity::*;
use crate::ir::*;

struct DefBlocks {
    def_block: PerEntity<Value, Block>,
}
impl DefBlocks {
    fn compute(body: &FunctionBody) -> Self {
        let mut def_block = PerEntity::default();
        for (block, data) in body.blocks.entries() {
            for &(_, param) in &data.params {
                def_block[param] = block;
            }
            for &inst in &data.insts {
                def_block[inst] = block;
            }
        }
        DefBlocks { def_block }
    }
}

pub fn run(body: &FunctionBody, cfg: &CFGInfo) {
    let def_blocks = DefBlocks::compute(body);

    // Visit only reachable blocks.
    for &block in cfg.rpo.values() {
        let data = &body.blocks[block];
        let validate = |value| {
            let value = body.resolve_alias(value);
            let def_block = def_blocks.def_block[value];
            assert!(
                cfg.dominates(def_block, block),
                "value {} defined in block {} used in block {}: def does not dominate use",
                value,
                def_block,
                block
            );
        };

        for (i, &(_, param)) in data.params.iter().enumerate() {
            match &body.values[param] {
                &ValueDef::BlockParam(param_block, param_idx, _) => {
                    assert_eq!(param_block, block);
                    assert_eq!(param_idx, i as u32);
                }
                _ => panic!(
                    "Bad blockparam value for param {} of {} ({}): {:?}",
                    i, block, param, body.values[param]
                ),
            }
        }

        for &inst in &data.insts {
            match &body.values[inst] {
                &ValueDef::Operator(_, args, _) => {
                    for &arg in &body.arg_pool[args] {
                        validate(arg);
                    }
                }
                &ValueDef::PickOutput(val, _, _) => {
                    validate(val);
                }
                &ValueDef::Alias(..) => {}
                &ValueDef::None | &ValueDef::Placeholder(_) | &ValueDef::BlockParam(..) => {}
            }
        }
        data.terminator.visit_uses(|u| validate(u));
    }
}
