#![no_main]
use libfuzzer_sys::fuzz_target;

use waffle::{FrontendOptions, Module};

fuzz_target!(|module: wasm_smith::Module| {
    let _ = env_logger::try_init();
    log::debug!("original module: {:?}", module);
    let orig_bytes = module.to_bytes();
    let parsed_module =
        Module::from_wasm_bytes(&orig_bytes[..], &FrontendOptions::default()).unwrap();
    let roundtrip_bytes = parsed_module.to_wasm_bytes().unwrap();
    if let Ok(filename) = std::env::var("ROUNDTRIP_WASM_SAVE") {
        std::fs::write(filename, &roundtrip_bytes[..]).unwrap();
    }
    let parsed_roundtrip_module =
        Module::from_wasm_bytes(&roundtrip_bytes[..], &FrontendOptions::default()).unwrap();
    let _ = parsed_roundtrip_module.to_wasm_bytes();
});
