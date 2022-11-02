//! WAFFLE Wasm analysis framework.

#![allow(dead_code)]

// Re-export wasmparser for easier use of the right version by our embedders.
pub use wasmparser;

mod backend;
mod cfg;
mod entity;
mod frontend;
mod ir;
mod op_traits;
mod ops;
mod passes;
pub use passes::rpo::reorder_into_rpo;

pub use ir::*;
pub use ops::Operator;
