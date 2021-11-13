//! WAFFLE Wasm analysis framework.

// Re-export wasmparser and wasmencoder for easier use of the right
// version by our embedders.
pub use wasmparser;
pub use wasm_encoder;

pub mod ir;
