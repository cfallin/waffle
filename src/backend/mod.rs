//! Backend: IR to Wasm.

mod stackify;
pub(crate) use stackify::*;

mod location;
pub(crate) use location::*;
