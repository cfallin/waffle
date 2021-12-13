//! Backend: IR to Wasm.

mod stackify;
pub(crate) use stackify::*;
mod locations;
pub(crate) use locations::*;
