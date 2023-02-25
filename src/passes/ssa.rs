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

    for (block, data) in body.blocks.entries() {
        let validate = |value| {
            let value = body.resolve_alias(value);
            let def_block = def_blocks.def_block[value];
            assert!(cfg.dominates(def_block, block));
        };

        for &inst in &data.insts {
            match &body.values[inst] {
                &ValueDef::Operator(_, ref args, _) => {
                    for &arg in args {
                        validate(arg);
                    }
                }
                &ValueDef::PickOutput(val, _, _) => {
                    validate(val);
                }
                &ValueDef::Trace(_, ref args) => {
                    for &arg in args {
                        validate(arg);
                    }
                }
                &ValueDef::Alias(..) => {}
                &ValueDef::None | &ValueDef::Placeholder(_) | &ValueDef::BlockParam(..) => {}
            }
        }
        data.terminator.visit_uses(|u| validate(u));
    }
}
