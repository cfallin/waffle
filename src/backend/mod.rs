//! Backend: IR to Wasm.

mod structured;
pub use structured::*;
mod serialize;
pub use serialize::*;
mod locations;
pub use locations::*;
mod r#final;
pub use r#final::*;

