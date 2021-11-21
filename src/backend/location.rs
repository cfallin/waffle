//! Decide locations for each Value.

use crate::cfg::CFGInfo;
use crate::ir::*;
use wasmparser::Type;

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
