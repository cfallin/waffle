//! Passes.

pub mod basic_opt;
pub mod dom_pass;
pub mod empty_blocks;
pub mod maxssa;
pub mod resolve_aliases;
pub mod ssa;
pub mod trace;

#[derive(Clone, Debug)]
pub struct Fuel {
    pub remaining: u64,
}
impl Fuel {
    pub fn consume(&mut self) -> bool {
        if self.remaining == 0 {
            false
        } else {
            self.remaining -= 1;
            true
        }
    }
}
