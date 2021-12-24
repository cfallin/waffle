//! Backend: IR to Wasm.

mod structured;
pub use structured::*;
mod use_count;
pub use use_count::*;
mod schedule;
pub use schedule::*;
mod serialize;
pub use serialize::*;

mod locations;

