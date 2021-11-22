//! Pass framework: skeletons for common kinds of passes over code.
//!
//! Terminology note: a "pass" is a readonly analysis of a function
//! body. It does not mutate code; it only traverses the code in a
//! certain order, possibly multiple times (to converge), in order to
//! compute some derived information.

pub mod dataflow;
pub use dataflow::*;
pub mod lattice;
pub use lattice::*;
