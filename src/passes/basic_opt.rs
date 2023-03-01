//! Basic optimizations: GVN and constant-propagation/folding.

use crate::cfg::CFGInfo;
use crate::interp::{const_eval, ConstVal};
use crate::ir::*;
use crate::passes::dom_pass::{dom_pass, DomtreePass};
use crate::scoped_map::ScopedMap;
use crate::Operator;

pub fn gvn(body: &mut FunctionBody, cfg: &CFGInfo) {
    dom_pass::<GVNPass>(
        body,
        cfg,
        &mut GVNPass {
            map: ScopedMap::default(),
        },
    );
}

#[derive(Debug)]
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
        ValueDef::Operator(op, ..) if op.is_pure() => true,
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
                value.update_uses(|val| *val = body.resolve_and_update_alias(*val));

                if let ValueDef::Operator(op, args, ..) = &value {
                    let arg_values = args
                        .iter()
                        .map(|&arg| match body.values[arg] {
                            ValueDef::Operator(Operator::I32Const { value }, _, _) => {
                                ConstVal::I32(value)
                            }
                            ValueDef::Operator(Operator::I64Const { value }, _, _) => {
                                ConstVal::I64(value)
                            }
                            ValueDef::Operator(Operator::F32Const { value }, _, _) => {
                                ConstVal::F32(value)
                            }
                            ValueDef::Operator(Operator::F64Const { value }, _, _) => {
                                ConstVal::F64(value)
                            }
                            _ => ConstVal::None,
                        })
                        .collect::<Vec<_>>();
                    let const_val = const_eval(op, &arg_values[..], None);
                    match const_val {
                        Some(ConstVal::I32(val)) => {
                            value = ValueDef::Operator(
                                Operator::I32Const { value: val },
                                vec![],
                                vec![Type::I32],
                            );
                            body.values[inst] = value.clone();
                        }
                        Some(ConstVal::I64(val)) => {
                            value = ValueDef::Operator(
                                Operator::I64Const { value: val },
                                vec![],
                                vec![Type::I64],
                            );
                            body.values[inst] = value.clone();
                        }
                        Some(ConstVal::F32(val)) => {
                            value = ValueDef::Operator(
                                Operator::F32Const { value: val },
                                vec![],
                                vec![Type::F32],
                            );
                            body.values[inst] = value.clone();
                        }
                        Some(ConstVal::F64(val)) => {
                            value = ValueDef::Operator(
                                Operator::F64Const { value: val },
                                vec![],
                                vec![Type::F64],
                            );
                            body.values[inst] = value.clone();
                        }
                        _ => {}
                    }
                }

                if let Some(value) = self.map.get(&value) {
                    body.set_alias(inst, *value);
                    i -= 1;
                    body.blocks[block].insts.remove(i);
                    continue;
                }

                self.map.insert(value, inst);
            }
        }
    }
}
