//! Use-count analysis.

use std::collections::VecDeque;

use crate::{FunctionBody, Value, ValueDef};
use fxhash::FxHashSet;

#[derive(Clone, Debug)]
pub struct UseCountAnalysis {
    pub(crate) toplevel: FxHashSet<Value>,
    pub(crate) use_count: Vec</* Value, */ usize>,
}

impl UseCountAnalysis {
    pub(crate) fn compute(f: &FunctionBody) -> UseCountAnalysis {
        let n_values = f.values.len();
        let mut counts = UseCountAnalysis {
            use_count: vec![0; n_values],
            toplevel: FxHashSet::default(),
        };

        let mut workqueue = VecDeque::new();
        let mut workqueue_set = FxHashSet::default();
        for block in 0..f.blocks.len() {
            for &value in &f.blocks[block].insts {
                if value != Value::undef() {
                    let value = f.resolve_alias(value);
                    if workqueue_set.insert(value) {
                        workqueue.push_back(value);
                    }
                    counts.toplevel.insert(value);
                }
            }
            f.blocks[block].terminator.visit_uses(|value| {
                if value != Value::undef() {
                    let value = f.resolve_alias(value);
                    if workqueue_set.insert(value) {
                        workqueue.push_back(value);
                    }
                }
            });

            while let Some(value) = workqueue.pop_front() {
                workqueue_set.remove(&value);
                counts.add(value);
                match &f.values[value.index()] {
                    &ValueDef::Alias(..) | &ValueDef::Arg(..) | &ValueDef::BlockParam(..) => {}
                    &ValueDef::Operator(_op, ref args) => {
                        for &arg in args {
                            if arg == Value::undef() {
                                continue;
                            }
                            let arg = f.resolve_alias(arg);
                            if counts.use_count[arg.index()] == 0 {
                                if workqueue_set.insert(arg) {
                                    workqueue.push_back(arg);
                                }
                            }
                        }
                    }
                    &ValueDef::PickOutput(value, _) => {
                        if value == Value::undef() {
                            continue;
                        }
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
