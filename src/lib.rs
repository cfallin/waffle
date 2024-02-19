//! WAFFLE Wasm analysis framework.

#![allow(dead_code)]

// Re-export wasmparser for easier use of the right version by our embedders.
pub use wasmparser;

mod backend;
pub mod cfg;
pub mod entity;
mod errors;
mod frontend;
mod ir;
pub mod op_traits;
mod ops;
pub mod passes;
pub mod pool;
mod scoped_map;

pub use errors::*;
pub use ir::*;
pub use ops::{Ieee32, Ieee64, MemoryArg, Operator};

mod interp;
pub use interp::*;

#[cfg(feature = "fuzzing")]
pub mod fuzzing;
