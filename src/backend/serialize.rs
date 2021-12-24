//! Serialization of the sea-of-nodes IR using a BlockOrder
//! Wasm-structured-control-flow result into actual order of operators
//! in Wasm function body. Contains everything needed to emit Wasm
//! except for value locations (and corresponding local spill/reloads).

use super::{
    structured::{BlockOrder, BlockOrderEntry},
    Schedule, UseCountAnalysis,
};
use crate::{
    cfg::CFGInfo, op_traits::op_rematerialize, BlockId, FunctionBody, Operator, Terminator, Value,
    ValueDef,
};
use fxhash::FxHashSet;

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
        params: Vec<(wasmparser::Type, Value)>,
        results: Vec<wasmparser::Type>,
    },
    StartLoop {
        header: BlockId,
        params: Vec<(wasmparser::Type, Value)>,
        results: Vec<wasmparser::Type>,
    },
    Br(SerializedBlockTarget),
    BrIf {
        if_true: SerializedBlockTarget,
        if_false: SerializedBlockTarget,
    },
    BrTable {
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
                ref default,
                ref targets,
            } => {
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
            &SerializedOperator::StartBlock { ref params, .. }
            | &SerializedOperator::StartLoop { ref params, .. } => {
                for &(_, value) in params {
                    w(value, 0);
                }
            }
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
    uses: &'a UseCountAnalysis,
    schedule: &'a Schedule,
    operators: Vec<SerializedOperator>,
}

impl SerializedBody {
    pub fn compute(f: &FunctionBody, cfg: &CFGInfo, order: &BlockOrder) -> SerializedBody {
        let uses = UseCountAnalysis::compute(f);
        let schedule = Schedule::compute(f, cfg, &uses);

        if log::log_enabled!(log::Level::Trace) {
            log::trace!("values:");
            for value in 0..f.values.len() {
                log::trace!(" * v{}: {:?}", value, f.values[value]);
            }
            log::trace!("schedule:");
            for value in 0..schedule.location.len() {
                log::trace!(" * v{}: {:?}", value, schedule.location[value]);
            }

            for block in 0..f.blocks.len() {
                log::trace!("block{}:", block);
                log::trace!(
                    " -> at top: {:?}",
                    schedule.compute_at_top_of_block.get(&block)
                );
                for &inst in &f.blocks[block].insts {
                    log::trace!(" -> toplevel: v{}", inst.index());
                    log::trace!(
                        "    -> after: {:?}",
                        schedule.compute_after_value.get(&inst)
                    );
                }
                log::trace!(" -> terminator: {:?}", f.blocks[block].terminator);
            }
        }

        let mut ctx = SerializedBodyContext {
            f,
            cfg,
            uses: &uses,
            schedule: &schedule,
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
            &BlockOrderEntry::StartBlock(header, ref params, ref results)
            | &BlockOrderEntry::StartLoop(header, ref params, ref results) => {
                let is_loop = match entry {
                    &BlockOrderEntry::StartLoop(..) => true,
                    _ => false,
                };

                if is_loop {
                    self.operators.push(SerializedOperator::StartLoop {
                        header,
                        params: params.clone(),
                        results: results.clone(),
                    });
                } else {
                    self.operators.push(SerializedOperator::StartBlock {
                        header,
                        params: params.clone(),
                        results: results.clone(),
                    });
                }

                // Save params that are on the stack into
                // locals. TODO: reuse one or more values immediately
                // if ready-to-schedule ops can use them.
                for &(_, value) in params.iter().rev() {
                    self.operators.push(SerializedOperator::Set(value, 0));
                }
            }
            &BlockOrderEntry::End => {
                self.operators.push(SerializedOperator::End);
            }
            &BlockOrderEntry::BasicBlock(block, ref targets) => {
                log::trace!("BlockOrderEntry: block{}", block);

                // Capture BlockParams.
                for &(_, param) in self.f.blocks[block].params.iter().rev() {
                    self.operators.push(SerializedOperator::Set(param, 0));
                }

                // Schedule ops. First handle the compute-at-top ones.
                if let Some(compute_at_top) = self.schedule.compute_at_top_of_block.get(&block) {
                    self.schedule_ops(None, &compute_at_top[..]);
                }

                // Next schedule all toplevels, and values ready to
                // schedule after each one.
                for &inst in &self.f.blocks[block].insts {
                    if let Some(after) = self.schedule.compute_after_value.get(&inst) {
                        self.schedule_ops(Some(inst), &after[..]);
                    } else {
                        self.schedule_ops(Some(inst), &[]);
                    }
                }

                // For each BlockOrderTarget, compute a SerializedBlockTarget.
                let targets = targets
                    .iter()
                    .map(|target| {
                        let mut rev_ops = vec![];
                        for &value in target.args.iter().rev() {
                            let value = self.f.resolve_alias(value);
                            self.push_value(value, &mut rev_ops);
                        }
                        rev_ops.reverse();
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
                        self.operators.extend(rev_ops);
                        self.operators
                            .push(SerializedOperator::BrTable { targets, default });
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

    fn schedule_ops(&mut self, toplevel: Option<Value>, values: &[Value]) {
        log::trace!("schedule_ops: toplevel {:?} values {:?}", toplevel, values);
        // Work backward, generating values in the appropriate order
        // on the stack if single-use.
        let mut rev_ops = vec![];
        let mut to_compute = values
            .iter()
            .chain(toplevel.iter())
            .cloned()
            .collect::<FxHashSet<_>>();
        for &value in values.iter().rev() {
            self.schedule_op(
                value,
                &mut rev_ops,
                /* leave_value_on_stack = */ false,
                &mut to_compute,
            );
        }
        if let Some(toplevel) = toplevel {
            self.schedule_op(
                toplevel,
                &mut rev_ops,
                /* leave_value_on_stack = */ false,
                &mut to_compute,
            );
        }
        rev_ops.reverse();
        log::trace!(
            "schedule_ops: toplevel {:?} values {:?} -> ops {:?}",
            toplevel,
            values,
            rev_ops
        );
        self.operators.extend(rev_ops.into_iter());
    }

    fn push_value(&mut self, v: Value, rev_ops: &mut Vec<SerializedOperator>) {
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

    fn schedule_op(
        &mut self,
        op: Value,
        rev_ops: &mut Vec<SerializedOperator>,
        leave_value_on_stack: bool,
        to_compute: &mut FxHashSet<Value>,
    ) {
        let op = self.f.resolve_alias(op);
        if !to_compute.remove(&op) {
            if leave_value_on_stack {
                self.push_value(op, rev_ops);
            }
            return;
        }

        let (operator, operands) = match &self.f.values[op.index()] {
            &ValueDef::Operator(op, ref operands) => (op, operands),
            _ => {
                return;
            }
        };

        // We're generating ops in reverse order. So we must first
        // store value.
        for i in 0..self.f.types[op.index()].len() {
            if !leave_value_on_stack {
                rev_ops.push(SerializedOperator::Set(op, i));
            } else {
                assert_eq!(i, 0);
                if self.uses.use_count[op.index()] > 1 {
                    rev_ops.push(SerializedOperator::Tee(op, i));
                }
            }
        }

        rev_ops.push(SerializedOperator::Operator(operator));

        // Now push the args in reverse order.
        for &arg in operands.iter().rev() {
            match &self.f.values[arg.index()] {
                &ValueDef::Operator(op, ..) => {
                    if op_rematerialize(&op) {
                        rev_ops.push(SerializedOperator::Operator(op));
                    } else if self.uses.use_count[arg.index()] == 1
                        && self.f.types[arg.index()].len() == 1
                    {
                        self.schedule_op(
                            arg, rev_ops, /* leave_on_stack = */ true, to_compute,
                        );
                    } else {
                        self.push_value(arg, rev_ops);
                    }
                }
                _ => {
                    self.push_value(arg, rev_ops);
                }
            }
        }
    }
}
