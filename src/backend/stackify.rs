//! Stackify implementation to produce structured control flow from an
//! arbitrary CFG.
//!
//! See the paper
//!
//! - Norman Ramsey. Beyond Relooper: recursive translation of
//!   unstructured control flow to structured control flow. In ICFP
//!   2022 (Functional Pearl). https://dl.acm.org/doi/10.1145/3547621
//!
//! for more details on how this algorithm works.

use crate::cfg::CFGInfo;
use crate::ir::{Block, FunctionBody};
use crate::passes::rpo::{RPOIndex, RPO};
use std::collections::{HashMap, HashSet};
