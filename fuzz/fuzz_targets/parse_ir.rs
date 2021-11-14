#![no_main]
use libfuzzer_sys::fuzz_target;

use waffle::frontend::wasm_to_ir;
use wasm_smith::Module;

fuzz_target!(|module: Module| {
    let _ = env_logger::try_init();
    let _parsed_module = wasm_to_ir(&module.to_bytes()[..]).unwrap();
});
