//! Backend: IR to Wasm.

mod schedule;
pub(crate) use schedule::*;
mod locations;
pub(crate) use locations::*;
