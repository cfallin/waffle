//! WAFFLE Wasm analysis framework.

#![allow(dead_code)]

// Re-export wasmparser for easier use of the right version by our embedders.
pub use wasmparser;

mod backend;
pub mod cfg;
pub mod entity;
mod frontend;
mod ir;
mod op_traits;
mod ops;
pub mod passes;
mod scoped_map;
mod errors;

pub use ir::*;
pub use ops::{Ieee32, Ieee64, Operator};
pub use errors::*;
