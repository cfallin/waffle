//! Liveness analysis.

use crate::{backward_pass, dataflow_use_def};
use crate::{ir::*, pass::*};

backward_pass!(
    Liveness,
    UnionBitSet,
    dataflow_use_def!(
        UnionBitSet,
        use: |u, lattice| {
            lattice.add(u.index() as usize);
        },
        def: |d, lattice| {
            lattice.remove(d.index() as usize);
        }
    )
);
