//! Serialization of the sea-of-nodes IR using a BlockOrder
//! Wasm-structured-control-flow result into actual order of operators
//! in Wasm function body. Contains everything needed to emit Wasm
//! except for value locations (and corresponding local spill/reloads).

use std::collections::VecDeque;

use fxhash::{FxHashMap, FxHashSet};

use super::{
    structured::{BlockOrder, BlockOrderEntry, BlockOrderTarget},
    CFGInfo,
};
use crate::{BlockId, FunctionBody, Operator, Terminator, Value, ValueDef};

/// A Wasm function body with a serialized sequence of operators that
/// mirror Wasm opcodes in every way *except* for locals corresponding
/// to SSA values. This is a sort of "pre-regalloc" representation of
/// the final code.
#[derive(Clone, Debug)]
pub struct SerializedBody {
    pub(crate) operators: Vec<SerializedOperator>,
}

#[derive(Clone, Debug)]
pub enum SerializedBlockTarget {
    Fallthrough(Vec<SerializedOperator>),
    Branch(usize, Vec<SerializedOperator>),
}

#[derive(Clone, Debug)]
pub enum SerializedOperator {
    StartBlock {
        header: BlockId,
        params: Vec<(wasmparser::Type, Value)>,
    },
    StartLoop {
        header: BlockId,
        params: Vec<(wasmparser::Type, Value)>,
    },
    Br(SerializedBlockTarget),
    BrIf {
        cond: Value,
        if_true: SerializedBlockTarget,
        if_false: SerializedBlockTarget,
    },
    BrTable {
        index: Value,
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
        mut r: R,
        mut w: W,
    ) {
        match self {
            &SerializedOperator::Br(ref target) => {
                target.visit_value_locals(&mut r, &mut w);
            }
            &SerializedOperator::BrIf {
                cond,
                ref if_true,
                ref if_false,
            } => {
                r(cond, 0);
                if_true.visit_value_locals(&mut r, &mut w);
                if_false.visit_value_locals(&mut r, &mut w);
            }
            &SerializedOperator::BrTable {
                index,
                ref default,
                ref targets,
            } => {
                r(index, 0);
                default.visit_value_locals(&mut r, &mut w);
                for target in targets {
                    target.visit_value_locals(&mut r, &mut w);
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
                    op.visit_value_locals(|value, i| r(value, i), |value, i| w(value, i));
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
            &BlockOrderEntry::StartBlock(header, ref params)
            | &BlockOrderEntry::StartLoop(header, ref params) => {
                let is_loop = match entry {
                    &BlockOrderEntry::StartLoop(..) => true,
                    _ => false,
                };

                if is_loop {
                    self.operators.push(SerializedOperator::StartLoop {
                        header,
                        params: params.clone(),
                    });
                } else {
                    self.operators.push(SerializedOperator::StartBlock {
                        header,
                        params: params.clone(),
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
                // Schedule ops. First handle the compute-at-top ones.
                if let Some(compute_at_top) = self.schedule.compute_at_top_of_block.get(&block) {
                    self.schedule_ops(None, &compute_at_top[..]);
                }

                // Next schedule all toplevels, and values ready to
                // schedule after each one.
                for &inst in &self.f.blocks[block].insts {
                    if let Some(after) = self.schedule.compute_after_value.get(&inst) {
                        self.schedule_ops(Some(inst), &after[..]);
                    }
                }

                // For each BlockOrderTarget, compute a SerializedBlockTarget.
                let targets = targets
                    .iter()
                    .map(|target| {
                        let mut rev_ops = vec![];
                        for &value in target.args.iter().rev() {
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
                        self.operators.push(SerializedOperator::BrIf {
                            cond,
                            if_true,
                            if_false,
                        });
                    }
                    &Terminator::Select { value, .. } => {
                        let mut iter = targets.into_iter();
                        let default = iter.next().unwrap();
                        let targets = iter.collect::<Vec<_>>();
                        self.operators.push(SerializedOperator::BrTable {
                            index: value,
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

    fn schedule_ops(&mut self, toplevel: Option<Value>, values: &[Value]) {
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
        for i in (0..self.f.types[op.index()].len()).rev() {
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
                &ValueDef::Operator(..) => {
                    if self.uses.use_count[arg.index()] == 1 && self.f.types[arg.index()].len() == 1
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

#[derive(Clone, Debug)]
pub struct UseCountAnalysis {
    toplevel: FxHashSet<Value>,
    use_count: Vec</* Value, */ usize>,
}

impl UseCountAnalysis {
    fn compute(f: &FunctionBody) -> UseCountAnalysis {
        let n_values = f.values.len();
        let mut counts = UseCountAnalysis {
            use_count: vec![0; n_values],
            toplevel: FxHashSet::default(),
        };

        let mut workqueue = VecDeque::new();
        let mut workqueue_set = FxHashSet::default();
        for block in 0..f.blocks.len() {
            for &value in &f.blocks[block].insts {
                let value = f.resolve_alias(value);
                if workqueue_set.insert(value) {
                    workqueue.push_back(value);
                }
                counts.toplevel.insert(value);
            }

            while let Some(value) = workqueue.pop_front() {
                workqueue_set.remove(&value);
                counts.add(value);
                match &f.values[value.index()] {
                    &ValueDef::Alias(..) | &ValueDef::Arg(..) | &ValueDef::BlockParam(..) => {}
                    &ValueDef::Operator(_op, ref args) => {
                        for &arg in args {
                            let arg = f.resolve_alias(arg);
                            if counts.use_count[arg.index()] == 0 {
                                if workqueue_set.insert(arg) {
                                    workqueue.push_back(arg);
                                }
                            }
                        }
                    }
                    &ValueDef::PickOutput(value, _) => {
                        let value = f.resolve_alias(value);
                        if counts.use_count[value.index()] == 0 {
                            if workqueue_set.insert(value) {
                                workqueue.push_back(value);
                            }
                        }
                    }
                    &ValueDef::Placeholder => {
                        panic!("Unresolved placeholder for value {}", value);
                    }
                }
            }
        }

        counts
    }

    fn add(&mut self, value: Value) {
        self.use_count[value.index()] += 1;
    }
}

#[derive(Clone, Debug, Default)]
pub struct Schedule {
    /// Output: location at which to compute each value.
    pub location: Vec</* Value, */ Location>,
    /// Output: for each toplevel value, all values that are computed
    /// after it is.
    pub compute_after_value: FxHashMap<Value, Vec<Value>>,
    /// Output: all values ready at the top of a given block.
    pub compute_at_top_of_block: FxHashMap<BlockId, Vec<Value>>,
}

pub struct SchedulerContext<'a> {
    /// The schedule we are constructing.
    schedule: &'a mut Schedule,
    /// In-progress state: for each value, the values that have one
    /// more ready input once that value is computed.
    waiting_on_value: FxHashMap<Value, Vec<Value>>,
    /// In-progress state: for each value, how many inputs need to
    /// become ready.
    remaining_inputs: FxHashMap<Value, usize>,
    /// In-progress state: all values that are ready to be scheduled.
    ready: Vec<Value>,
    /// Input context: CFG.
    cfg: &'a CFGInfo,
    /// Input context: function body.
    f: &'a FunctionBody,
}

/// Locations are denoted by top-level values (those in `insts`),
/// which are those with a side-effect; the sea-of-nodes
/// representation for all other value nodes allows them to be
/// computed anywhere dominated by all operands and that dominates all
/// uses, so we have significant flexibility. We denote a location as
/// "after a toplevel", then in the second pass where we actually
/// generate operators according to stack discipline, we resolve the
/// order for all values at a given toplevel.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location {
    /// At a separate top-level location.
    Toplevel,
    /// After a given value.
    After(Value),
    /// At the top of a given block.
    BlockTop(BlockId),
    /// Not yet scheduled.
    None,
}

impl Schedule {
    pub fn compute(f: &FunctionBody, cfg: &CFGInfo, uses: &UseCountAnalysis) -> Self {
        let mut schedule = Schedule::default();
        schedule.location = vec![Location::None; f.values.len()];

        let mut ctx = SchedulerContext {
            schedule: &mut schedule,
            f,
            cfg,
            waiting_on_value: FxHashMap::default(),
            remaining_inputs: FxHashMap::default(),
            ready: vec![],
        };

        // Prepare the "waiting on value", "remaining inputs", and
        // "ready" vectors.
        for (value, value_def) in f.values() {
            if uses.use_count[value.index()] == 0 {
                continue;
            }
            match value_def {
                &ValueDef::Operator(_, ref operands) => {
                    if operands.len() == 0 {
                        ctx.ready.push(value);
                    } else {
                        ctx.remaining_inputs.insert(value, operands.len());
                        for &input in operands {
                            let input = f.resolve_alias(input);
                            ctx.waiting_on_value
                                .entry(input)
                                .or_insert_with(|| vec![])
                                .push(value);
                        }
                    }
                }
                &ValueDef::Alias(v) | &ValueDef::PickOutput(v, _) => {
                    let v = f.resolve_alias(v);
                    ctx.remaining_inputs.insert(v, 1);
                    ctx.waiting_on_value
                        .entry(v)
                        .or_insert_with(|| vec![])
                        .push(value);
                }
                _ => {}
            }
        }

        // Traverse blocks in RPO. When we schedule a given op, we've
        // already scheduled all of its operands, so we can find the
        // right place for it without any sort of backtracking or
        // fixpoint convergence.
        //
        // - Values in `insts` (toplevel operations)
        //   are scheduled at their locations. All side-effecting ops
        //   are in this category, and hence never experience
        //   code-motion relative to other side-effecting ops or
        //   control flow.
        //
        // - Otherwise, values are scheduled after their last operand
        //   is ready. All operands must have been computed by the
        //   time we reach a given operator in RPO, and each operand's
        //   scheduled site must dominate the current location
        //   (toplevel value). Because the dominance relation forms a
        //   tree structure (the domtree), for any two operand def
        //   sites X and Y to the current location L, given X dom L
        //   and Y dom L, either X dom Y or Y dom X. Thus, consider
        //   the current-best and each new operand in pairs, and pick
        //   the one that is dominated by the other.

        for &block in cfg.postorder.iter().rev() {
            for &(_, param) in &f.blocks[block].params {
                ctx.wake_dependents(param);
            }
            for &inst in &f.blocks[block].insts {
                ctx.sched_toplevel(inst);
            }
        }

        schedule
    }
}

impl<'a> SchedulerContext<'a> {
    fn sched_toplevel(&mut self, v: Value) {
        assert_eq!(self.schedule.location[v.index()], Location::None);
        self.schedule.location[v.index()] = Location::Toplevel;
        self.wake_dependents(v);
    }

    fn sched_ready_after_value(&mut self, v: Value) {
        while !self.ready.is_empty() {
            for ready in std::mem::take(&mut self.ready) {
                self.schedule.location[ready.index()] = Location::After(v);
                self.schedule
                    .compute_after_value
                    .entry(v)
                    .or_insert_with(|| vec![])
                    .push(ready);
                self.wake_dependents(ready);
            }
        }
    }

    fn sched_ready_at_block_top(&mut self, block: BlockId) {
        while !self.ready.is_empty() {
            for ready in std::mem::take(&mut self.ready) {
                self.schedule.location[ready.index()] = Location::BlockTop(block);
                self.schedule
                    .compute_at_top_of_block
                    .entry(block)
                    .or_insert_with(|| vec![])
                    .push(ready);
                self.wake_dependents(ready);
            }
        }
    }

    fn wake_dependents(&mut self, v: Value) {
        let dependents = self.waiting_on_value.remove(&v).unwrap_or_default();
        for dependent in dependents {
            let remaining = self.remaining_inputs.get_mut(&dependent).unwrap();
            *remaining -= 1;
            if *remaining == 0 {
                self.remaining_inputs.remove(&dependent);
                self.ready.push(dependent);
                self.wake_dependents(dependent);
            }
        }
    }
}
