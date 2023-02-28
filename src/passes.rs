//! Passes.

pub mod basic_opt;
pub mod dom_pass;
pub mod empty_blocks;
pub mod maxssa;
pub mod remove_phis;
pub mod resolve_aliases;
pub mod ssa;
pub mod trace;

#[derive(Clone, Debug)]
pub struct Fuel {
    pub remaining: u64,
    pub consumed: u64,
}
impl Fuel {
    pub fn consume(&mut self) -> bool {
        self.consumed += 1;
        if self.remaining == u64::MAX {
            return true;
        }
        if self.remaining == 0 {
            false
        } else {
            self.remaining -= 1;
            true
        }
    }
    pub fn infinite() -> Fuel {
        Fuel {
            consumed: 0,
            remaining: u64::MAX,
        }
    }
}
