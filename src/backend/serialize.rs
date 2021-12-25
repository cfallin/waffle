//! Serialization of the sea-of-nodes IR using a BlockOrder
//! Wasm-structured-control-flow result into actual order of operators
//! in Wasm function body. Contains everything needed to emit Wasm
//! except for value locations (and corresponding local spill/reloads).

use super::structured::{BlockOrder, BlockOrderEntry};
use crate::{
    cfg::CFGInfo, op_traits::op_rematerialize, BlockId, FunctionBody, Operator, Terminator, Value,
    ValueDef,
};

/// A Wasm function body with a serialized sequence of operators that
/// mirror Wasm opcodes in every way *except* for locals corresponding
/// to SSA values. This is a sort of "pre-regalloc" representation of
/// the final code.
#[derive(Clone, Debug)]
pub struct SerializedBody {
    pub(crate) operators: Vec<SerializedOperator>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SerializedBlockTarget {
    Fallthrough(Vec<SerializedOperator>),
    Branch(usize, Vec<SerializedOperator>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SerializedOperator {
    StartBlock {
        header: BlockId,
    },
    StartLoop {
        header: BlockId,
    },
    Br(SerializedBlockTarget),
    BrIf {
        if_true: SerializedBlockTarget,
        if_false: SerializedBlockTarget,
    },
    BrTable {
        index_ops: Vec<SerializedOperator>,
        targets: Vec<SerializedBlockTarget>,
        default: SerializedBlockTarget,
    },
    /// Compute the given value. Stack discipline will be maintained:
    /// all operands will be computed or fetched via `Get` and all
    /// produced results will be used directly or stored via `Set`.
    Operator(Operator),
    /// Get the given value from the local corresponding to the
    /// `Value`'s n'th result.
    Get(Value, usize),
    /// Set the local corresponding to the `Value`'s n'th result,
    /// consuming the value on the stack.
    Set(Value, usize),
    /// Set the value, like `Set`, but without consuming it from the
    /// stack.
    Tee(Value, usize),
    /// Get the given function argument.
    GetArg(usize),
    End,
}

impl SerializedOperator {
    pub fn visit_value_locals<R: FnMut(Value, usize), W: FnMut(Value, usize)>(
        &self,
        r: &mut R,
        w: &mut W,
    ) {
        match self {
            &SerializedOperator::Br(ref target) => {
                target.visit_value_locals(r, w);
            }
            &SerializedOperator::BrIf {
                ref if_true,
                ref if_false,
            } => {
                if_true.visit_value_locals(r, w);
                if_false.visit_value_locals(r, w);
            }
            &SerializedOperator::BrTable {
                ref index_ops,
                ref default,
                ref targets,
            } => {
                for index_op in index_ops {
                    index_op.visit_value_locals(r, w);
                }
                default.visit_value_locals(r, w);
                for target in targets {
                    target.visit_value_locals(r, w);
                }
            }
            &SerializedOperator::Get(v, i) => {
                r(v, i);
            }
            &SerializedOperator::Set(v, i) | &SerializedOperator::Tee(v, i) => {
                w(v, i);
            }
            &SerializedOperator::StartBlock { .. } | &SerializedOperator::StartLoop { .. } => {}
            &SerializedOperator::GetArg(..)
            | &SerializedOperator::Operator(..)
            | &SerializedOperator::End => {}
        }
    }
}

impl SerializedBlockTarget {
    fn visit_value_locals<R: FnMut(Value, usize), W: FnMut(Value, usize)>(
        &self,
        r: &mut R,
        w: &mut W,
    ) {
        match self {
            &SerializedBlockTarget::Branch(_, ref ops)
            | &SerializedBlockTarget::Fallthrough(ref ops) => {
                for op in ops {
                    op.visit_value_locals(r, w);
                }
            }
        }
    }
}

struct SerializedBodyContext<'a> {
    f: &'a FunctionBody,
    cfg: &'a CFGInfo,
    operators: Vec<SerializedOperator>,
}

impl SerializedBody {
    pub fn compute(f: &FunctionBody, cfg: &CFGInfo, order: &BlockOrder) -> SerializedBody {
        if log::log_enabled!(log::Level::Trace) {
            log::trace!("values:");
            for value in 0..f.values.len() {
                log::trace!(" * v{}: {:?}", value, f.values[value]);
            }

            for block in 0..f.blocks.len() {
                log::trace!("block{}:", block);
                for &inst in &f.blocks[block].insts {
                    log::trace!(" -> v{}", inst.index());
                }
                log::trace!(" -> terminator: {:?}", f.blocks[block].terminator);
            }
        }

        let mut ctx = SerializedBodyContext {
            f,
            cfg,
            operators: vec![],
        };
        for entry in &order.entries {
            ctx.compute_entry(entry);
        }
        SerializedBody {
            operators: ctx.operators,
        }
    }
}

impl<'a> SerializedBodyContext<'a> {
    fn compute_entry(&mut self, entry: &BlockOrderEntry) {
        match entry {
            &BlockOrderEntry::StartBlock(header) | &BlockOrderEntry::StartLoop(header) => {
                let is_loop = match entry {
                    &BlockOrderEntry::StartLoop(..) => true,
                    _ => false,
                };

                if is_loop {
                    self.operators
                        .push(SerializedOperator::StartLoop { header });
                } else {
                    self.operators
                        .push(SerializedOperator::StartBlock { header });
                }
            }
            &BlockOrderEntry::End => {
                self.operators.push(SerializedOperator::End);
            }
            &BlockOrderEntry::BasicBlock(block, ref targets) => {
                log::trace!("BlockOrderEntry: block{}", block);

                // Compute insts' values in sequence.
                for &inst in &self.f.blocks[block].insts {
                    let mut rev_ops = vec![];
                    self.emit_inst(inst, &mut rev_ops);
                    rev_ops.reverse();
                    self.operators.extend(rev_ops);
                }

                // For each BlockOrderTarget, compute a SerializedBlockTarget.
                let targets = targets
                    .iter()
                    .map(|target| {
                        log::trace!("target: {:?}", target);

                        let mut rev_ops = vec![];

                        // Store into block param values.
                        for &(_, value) in &self.f.blocks[target.target].params {
                            rev_ops.push(SerializedOperator::Set(value, 0));
                        }

                        // Load from branch operator's args.
                        for &value in target.args.iter().rev() {
                            let value = self.f.resolve_alias(value);
                            self.push_value(value, &mut rev_ops);
                        }

                        rev_ops.reverse();
                        log::trace!(" -> ops: {:?}", rev_ops);

                        match target.relative_branch {
                            Some(branch) => SerializedBlockTarget::Branch(branch, rev_ops),
                            None => SerializedBlockTarget::Fallthrough(rev_ops),
                        }
                    })
                    .collect::<Vec<_>>();

                // Finally, generate branch ops.
                match &self.f.blocks[block].terminator {
                    &Terminator::Br { .. } => {
                        let target = targets.into_iter().next().unwrap();
                        self.operators.push(SerializedOperator::Br(target));
                    }
                    &Terminator::CondBr { cond, .. } => {
                        let mut iter = targets.into_iter();
                        let if_true = iter.next().unwrap();
                        let if_false = iter.next().unwrap();
                        let mut rev_ops = vec![];
                        let cond = self.f.resolve_alias(cond);
                        self.push_value(cond, &mut rev_ops);
                        rev_ops.reverse();
                        self.operators.extend(rev_ops);
                        self.operators
                            .push(SerializedOperator::BrIf { if_true, if_false });
                    }
                    &Terminator::Select { value, .. } => {
                        let mut iter = targets.into_iter();
                        let default = iter.next().unwrap();
                        let targets = iter.collect::<Vec<_>>();
                        let mut rev_ops = vec![];
                        let value = self.f.resolve_alias(value);
                        self.push_value(value, &mut rev_ops);
                        rev_ops.reverse();
                        self.operators.push(SerializedOperator::BrTable {
                            index_ops: rev_ops,
                            targets,
                            default,
                        });
                    }
                    &Terminator::Return { ref values, .. } => {
                        let mut rev_ops = vec![];
                        for &value in values.iter().rev() {
                            self.push_value(value, &mut rev_ops);
                        }
                        rev_ops.reverse();
                        self.operators.extend(rev_ops.into_iter());
                        self.operators
                            .push(SerializedOperator::Operator(Operator::Return));
                    }
                    &Terminator::None => {
                        self.operators
                            .push(SerializedOperator::Operator(Operator::Unreachable));
                    }
                }
            }
        }
    }
    fn push_value(&mut self, v: Value, rev_ops: &mut Vec<SerializedOperator>) {
        let v = self.f.resolve_alias(v);
        match &self.f.values[v.index()] {
            &ValueDef::PickOutput(v, i) => {
                rev_ops.push(SerializedOperator::Get(v, i));
            }
            &ValueDef::Arg(i) => {
                rev_ops.push(SerializedOperator::GetArg(i));
            }
            &ValueDef::Operator(op, ..) if op_rematerialize(&op) => {
                rev_ops.push(SerializedOperator::Operator(op));
            }
            _ => {
                rev_ops.push(SerializedOperator::Get(v, 0));
            }
        }
    }

    fn emit_inst(&mut self, inst: Value, rev_ops: &mut Vec<SerializedOperator>) {
        let (operator, operands) = match &self.f.values[inst.index()] {
            &ValueDef::Operator(op, ref operands) => (op, operands),
            _ => {
                return;
            }
        };

        // We're generating ops in reverse order. So we must first
        // store value.
        for i in 0..self.f.types[inst.index()].len() {
            rev_ops.push(SerializedOperator::Set(inst, i));
        }

        rev_ops.push(SerializedOperator::Operator(operator));

        // Now push the args in reverse order.
        for &arg in operands.iter().rev() {
            let arg = self.f.resolve_alias(arg);
            self.push_value(arg, rev_ops);
        }
    }
}
