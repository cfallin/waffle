//! WAFFLE Wasm analysis framework.

// Re-export wasmparser and wasmencoder for easier use of the right
// version by our embedders.
pub use wasm_encoder;
pub use wasmparser;

pub mod frontend;
pub mod ir;
