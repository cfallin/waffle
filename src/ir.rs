//! Intermediate representation for Wasm.

use crate::entity;

pub use wasmparser::Type;

entity!(Signature, "sig");
entity!(Func, "func");
entity!(Block, "block");
entity!(Local, "local");
entity!(Global, "global");
entity!(Table, "table");
entity!(Memory, "memory");
entity!(Value, "value");

mod module;
pub use module::*;
mod func;
pub use func::*;
mod value;
pub use value::*;
