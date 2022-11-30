//! Basic optimizations: GVN and constant-propagation/folding.

use crate::cfg::CFGInfo;
use crate::ir::*;
use crate::passes::dom_pass::{dom_pass, DomtreePass};
use crate::scoped_map::ScopedMap;
use crate::Operator;

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
        ValueDef::Operator(op, ..) if op.is_pure() => true,
        _ => false,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ConstVal {
    I32(u32),
    I64(u64),
    None,
}

fn const_eval(op: &Operator, vals: &[ConstVal]) -> Option<ConstVal> {
    match (op, vals) {
        (Operator::I32Const { value }, []) => Some(ConstVal::I32(*value as u32)),
        (Operator::I64Const { value }, []) => Some(ConstVal::I64(*value as u64)),
        (Operator::I32Eqz, [ConstVal::I32(a)]) => Some(ConstVal::I32(if *a == 0 { 1 } else { 0 })),
        (Operator::I32Eq, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if a == b { 1 } else { 0 }))
        }
        (Operator::I32Ne, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if a != b { 1 } else { 0 }))
        }
        (Operator::I32LtS, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if (*a as i32) < (*b as i32) { 1 } else { 0 }))
        }
        (Operator::I32LtU, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if a < b { 1 } else { 0 }))
        }
        (Operator::I32GtS, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if (*a as i32) > (*b as i32) { 1 } else { 0 }))
        }
        (Operator::I32GtU, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if a > b { 1 } else { 0 }))
        }
        (Operator::I32LeS, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if (*a as i32) <= (*b as i32) {
                1
            } else {
                0
            }))
        }
        (Operator::I32LeU, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if a <= b { 1 } else { 0 }))
        }
        (Operator::I32GeS, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if (*a as i32) >= (*b as i32) {
                1
            } else {
                0
            }))
        }
        (Operator::I32GeU, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(if a >= b { 1 } else { 0 }))
        }
        (Operator::I64Eqz, [ConstVal::I64(a)]) => Some(ConstVal::I32(if *a == 0 { 1 } else { 0 })),
        (Operator::I64Eq, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if a == b { 1 } else { 0 }))
        }
        (Operator::I64Ne, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if a != b { 1 } else { 0 }))
        }
        (Operator::I64LtS, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if (*a as i64) < (*b as i64) { 1 } else { 0 }))
        }
        (Operator::I64LtU, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if a < b { 1 } else { 0 }))
        }
        (Operator::I64GtS, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if (*a as i64) > (*b as i64) { 1 } else { 0 }))
        }
        (Operator::I64GtU, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if a > b { 1 } else { 0 }))
        }
        (Operator::I64LeS, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if (*a as i64) <= (*b as i64) {
                1
            } else {
                0
            }))
        }
        (Operator::I64LeU, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if a <= b { 1 } else { 0 }))
        }
        (Operator::I64GeS, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if (*a as i64) >= (*b as i64) {
                1
            } else {
                0
            }))
        }
        (Operator::I64GeU, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I32(if a >= b { 1 } else { 0 }))
        }

        (Operator::I32Add, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(a.wrapping_add(*b)))
        }
        (Operator::I32Sub, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(a.wrapping_sub(*b)))
        }
        (Operator::I32Mul, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(a.wrapping_mul(*b)))
        }
        (Operator::I32And, [ConstVal::I32(a), ConstVal::I32(b)]) => Some(ConstVal::I32(a & b)),
        (Operator::I32Or, [ConstVal::I32(a), ConstVal::I32(b)]) => Some(ConstVal::I32(a | b)),
        (Operator::I32Xor, [ConstVal::I32(a), ConstVal::I32(b)]) => Some(ConstVal::I32(a ^ b)),
        (Operator::I32Shl, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(a.wrapping_shl(*b)))
        }
        (Operator::I32ShrS, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32((*a as i32).wrapping_shr(*b) as u32))
        }

        (Operator::I32ShrU, [ConstVal::I32(a), ConstVal::I32(b)]) => {
            Some(ConstVal::I32(a.wrapping_shr(*b)))
        }

        (Operator::I64Add, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I64(a.wrapping_add(*b)))
        }
        (Operator::I64Sub, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I64(a.wrapping_sub(*b)))
        }
        (Operator::I64Mul, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I64(a.wrapping_mul(*b)))
        }
        (Operator::I64And, [ConstVal::I64(a), ConstVal::I64(b)]) => Some(ConstVal::I64(a & b)),
        (Operator::I64Or, [ConstVal::I64(a), ConstVal::I64(b)]) => Some(ConstVal::I64(a | b)),
        (Operator::I64Xor, [ConstVal::I64(a), ConstVal::I64(b)]) => Some(ConstVal::I64(a ^ b)),
        (Operator::I64Shl, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I64(a.wrapping_shl(*b as u32)))
        }
        (Operator::I64ShrS, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I64((*a as i64).wrapping_shr(*b as u32) as u64))
        }

        (Operator::I64ShrU, [ConstVal::I64(a), ConstVal::I64(b)]) => {
            Some(ConstVal::I64(a.wrapping_shr(*b as u32)))
        }

        (Operator::I32Extend8S, [ConstVal::I32(a)]) => Some(ConstVal::I32(*a as i8 as i32 as u32)),
        (Operator::I32Extend16S, [ConstVal::I32(a)]) => {
            Some(ConstVal::I32(*a as i16 as i32 as u32))
        }
        (Operator::I64Extend8S, [ConstVal::I64(a)]) => Some(ConstVal::I64(*a as i8 as i64 as u64)),
        (Operator::I64Extend16S, [ConstVal::I64(a)]) => {
            Some(ConstVal::I64(*a as i16 as i64 as u64))
        }
        (Operator::I64Extend32S, [ConstVal::I64(a)]) => {
            Some(ConstVal::I64(*a as i32 as i64 as u64))
        }
        (Operator::Select, [x, y, ConstVal::I32(k)]) => Some(if *k != 0 { *x } else { *y }),

        _ => None,
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

                if let ValueDef::Operator(op, args, ..) = &value {
                    let arg_values = args
                        .iter()
                        .map(|&arg| match body.values[arg] {
                            ValueDef::Operator(Operator::I32Const { value }, _, _) => {
                                ConstVal::I32(value as u32)
                            }
                            ValueDef::Operator(Operator::I64Const { value }, _, _) => {
                                ConstVal::I64(value as u64)
                            }
                            _ => ConstVal::None,
                        })
                        .collect::<Vec<_>>();
                    let const_val = const_eval(op, &arg_values[..]);
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
