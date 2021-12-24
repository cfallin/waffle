//! Op scheduling.

use fxhash::FxHashMap;

use super::UseCountAnalysis;
use crate::{cfg::CFGInfo, op_traits::op_rematerialize, BlockId, FunctionBody, Value, ValueDef};

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

        log::trace!("f: {:?}", f);
        log::trace!("cfg: {:?}", cfg);
        log::trace!("uses: {:?}", uses);

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
            if uses.toplevel.contains(&value) {
                continue;
            }
            match value_def {
                &ValueDef::Operator(op, ref operands) => {
                    if operands.iter().any(|&value| value == Value::undef()) {
                        continue;
                    }

                    if operands.len() == 0 {
                        if !op_rematerialize(&op) {
                            log::trace!("immediately ready: v{}", value.index());
                            ctx.ready.push(value);
                        }
                    } else {
                        let mut remaining = 0;
                        for &input in operands {
                            let input = f.resolve_alias(input);

                            match &f.values[input.index()] {
                                &ValueDef::Operator(ref op, ..) if op_rematerialize(op) => {
                                    continue;
                                }
                                &ValueDef::Arg(..) => {
                                    continue;
                                }
                                _ => {}
                            }

                            log::trace!("v{} waiting on v{}", value.index(), input.index());
                            ctx.waiting_on_value
                                .entry(input)
                                .or_insert_with(|| vec![])
                                .push(value);
                            remaining += 1;
                        }
                        if remaining > 0 {
                            ctx.remaining_inputs.insert(value, remaining);
                        } else {
                            ctx.ready.push(value);
                        }
                    }
                }
                &ValueDef::Alias(v) | &ValueDef::PickOutput(v, _) => {
                    let v = f.resolve_alias(v);
                    ctx.remaining_inputs.insert(value, 1);
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
                log::trace!("block{}: param v{}", block, param.index());
                ctx.wake_dependents(param);
            }
            ctx.sched_ready_at_block_top(block);
            for &inst in &f.blocks[block].insts {
                log::trace!("block{}: toplevel v{}", block, inst.index());
                ctx.sched_toplevel(inst);
                ctx.sched_ready_after_value(inst);
            }
        }

        schedule
    }
}

impl<'a> SchedulerContext<'a> {
    fn sched_toplevel(&mut self, v: Value) {
        log::trace!("sched_toplevel: v{}", v.index());
        assert_eq!(self.schedule.location[v.index()], Location::None);
        self.schedule.location[v.index()] = Location::Toplevel;
        self.wake_dependents(v);
    }

    fn sched_ready_after_value(&mut self, v: Value) {
        log::trace!("sched_ready_after_value: toplevel v{}", v.index());
        while !self.ready.is_empty() {
            for ready in std::mem::take(&mut self.ready) {
                log::trace!(
                    "sched_ready_after_value: toplevel v{} -> v{} now ready",
                    v.index(),
                    ready.index()
                );
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
        log::trace!("ready_at_block_top: block{}", block);
        while !self.ready.is_empty() {
            for ready in std::mem::take(&mut self.ready) {
                log::trace!(
                    "ready_at_block_top: block{} -> ready: v{}",
                    block,
                    ready.index()
                );
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
        log::trace!("wake_dependents: v{}", v.index());
        let dependents = self.waiting_on_value.remove(&v).unwrap_or_default();
        for dependent in dependents {
            log::trace!(" -> v{} wakes dependent v{}", v.index(), dependent.index(),);
            let remaining = self.remaining_inputs.get_mut(&dependent).unwrap();
            *remaining -= 1;
            log::trace!(" -> remaining now {}", *remaining);
            if *remaining == 0 {
                self.remaining_inputs.remove(&dependent);
                self.ready.push(dependent);
                self.wake_dependents(dependent);
            }
        }
    }
}
