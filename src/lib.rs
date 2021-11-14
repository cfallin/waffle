//! WAFFLE Wasm analysis framework.

// Re-export wasmparser and wasmencoder for easier use of the right
// version by our embedders.
pub use wasm_encoder;
pub use wasmparser;

mod frontend;
mod ir;
mod localssa;
mod op_traits;

pub use ir::*;
