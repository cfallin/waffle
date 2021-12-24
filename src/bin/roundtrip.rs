//! Roundtrip utility.

use std::io::{Read, Write};
use waffle::Module;

fn main() {
    let _ = env_logger::try_init();
    let mut bytes = vec![];
    std::io::stdin().read_to_end(&mut bytes).unwrap();
    let module = Module::from_wasm_bytes(&bytes).unwrap();
    let new_bytes = module.to_wasm_bytes();
    std::io::stdout().write(&new_bytes[..]).unwrap();
}
