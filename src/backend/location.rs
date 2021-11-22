//! Decide locations for each Value.

use crate::cfg::CFGInfo;
use crate::ir::*;
use fxhash::FxHashMap;
use wasmparser::Type;

/// Pass to compute reference counts for every value.
struct UseCounts {
    counts: FxHashMap<Value, usize>,
}

impl UseCounts {
    fn new(f: &FunctionBody) -> UseCounts {
        let mut counts = FxHashMap::default();
        for block in &f.blocks {
            block.visit_uses(|value| {
                *counts.entry(value).or_insert(0) += 1;
            });
        }
        UseCounts { counts }
    }

    fn count(&self, value: Value) -> usize {
        *self.counts.get(&value).unwrap_or(&0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location {
    // Store in a local.
    Local(usize),
    // Immediately generate at a single use-site.
    Stack,
    // No location.
    None,
}

#[derive(Clone, Debug)]
pub struct Locations {
    next_local: usize,
    extra_locals: Vec<Type>,
    locations: Vec</* Value, */ Location>,
}

impl Locations {
    fn compute(_f: &FunctionBody, _cfg: &CFGInfo) -> Self {
        todo!()
    }
}
