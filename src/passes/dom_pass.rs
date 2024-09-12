//! Simple framework for a domtree-based pass.

use crate::cfg::CFGInfo;
use crate::ir::{Block, FunctionBody};

pub trait DomtreePass {
    fn enter(&mut self, _block: Block, _body: &mut FunctionBody) {}
    fn leave(&mut self, _block: Block, _body: &mut FunctionBody) {}
}

pub fn dom_pass<P: DomtreePass>(body: &mut FunctionBody, cfg: &CFGInfo, pass: &mut P) {
    visit::<P>(body, cfg, pass, body.entry);
}

fn visit<P: DomtreePass>(body: &mut FunctionBody, cfg: &CFGInfo, pass: &mut P, block: Block) {
    pass.enter(block, body);
    for child in cfg.dom_children(block) {
        visit(body, cfg, pass, child);
    }
    pass.leave(block, body);
}
