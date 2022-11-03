//! Basic optimizations: GVN and constant-propagation/folding.

use crate::cfg::CFGInfo;
use crate::ir::*;
use crate::op_traits::is_pure;
use crate::passes::dom_pass::{dom_pass, DomtreePass};
use crate::scoped_map::ScopedMap;

pub fn gvn(body: &mut FunctionBody, cfg: &CFGInfo) {
    dom_pass::<GVNPass>(body, cfg, &mut GVNPass::default());
}

#[derive(Clone, Debug, Default)]
struct GVNPass {
    map: ScopedMap<ValueDef, Value>,
}

impl DomtreePass for GVNPass {
    fn enter(&mut self, block: Block, body: &mut FunctionBody) {
        self.map.push_level();
        self.optimize(block, body);
    }

    fn leave(&mut self, _block: Block, _body: &mut FunctionBody) {
        self.map.pop_level();
    }
}

fn value_is_pure(value: Value, body: &FunctionBody) -> bool {
    match body.values[value] {
        ValueDef::Operator(op, ..) if is_pure(&op) => true,
        _ => false,
    }
}

impl GVNPass {
    fn optimize(&mut self, block: Block, body: &mut FunctionBody) {
        let mut i = 0;
        while i < body.blocks[block].insts.len() {
            let inst = body.blocks[block].insts[i];
            i += 1;
            if value_is_pure(inst, body) {
                let mut value = body.values[inst].clone();
                value.update_uses(|val| *val = body.resolve_alias(*val));
                if let Some(value) = self.map.get(&value) {
                    body.set_alias(inst, *value);
                    i -= 1;
                    body.blocks[block].insts.remove(i);
                } else {
                    self.map.insert(value, inst);
                }
            }
        }
    }
}
