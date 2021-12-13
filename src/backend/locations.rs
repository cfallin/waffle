//! Location assignment (pseudo-regalloc) for SSA values onto
//! locals/operand-stack values.

use crate::LocalId;

pub enum Location {
    Local(LocalId),
    // TODO: use operand stack
}
