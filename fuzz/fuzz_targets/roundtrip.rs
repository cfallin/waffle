#![no_main]
use libfuzzer_sys::fuzz_target;

use waffle::Module;

fuzz_target!(|module: wasm_smith::Module| {
    let _ = env_logger::try_init();
    log::debug!("original module: {:?}", module);
    let orig_bytes = module.to_bytes();
    let parsed_module = Module::from_wasm_bytes(&orig_bytes[..]).unwrap();
    let _ = parsed_module.to_wasm_bytes();
});
