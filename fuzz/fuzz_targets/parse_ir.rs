#![no_main]
use libfuzzer_sys::fuzz_target;

use waffle::{FrontendOptions, Module};

fuzz_target!(|module: wasm_smith::Module| {
    let _ = env_logger::try_init();
    let _parsed_module =
        Module::from_wasm_bytes(&module.to_bytes()[..], &FrontendOptions::default()).unwrap();
});
