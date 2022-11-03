//! Basic optimizations: GVN and constant-propagation/folding.

use crate::ir::*;
use crate::passes::dom_pass;
use crate::scoped_map::ScopedMap;
use crate::cfg::CFGInfo;

pub fn gvn(body: &mut FunctionBody, cfg: &CFGInfo) {
    let mut map: ScopedMap<ValueDef, Value> = ScopedMap::new();
}
