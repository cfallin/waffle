#![no_main]
use libfuzzer_sys::fuzz_target;

use waffle::{FrontendError, FrontendOptions, Module, OptOptions};

fuzz_target!(|module: wasm_smith::Module| {
    let _ = env_logger::try_init();
    log::debug!("original module: {:?}", module);
    let orig_bytes = module.to_bytes();
    let mut parsed_module =
        match Module::from_wasm_bytes(&orig_bytes[..], &FrontendOptions::default()) {
            Ok(m) => m,
            Err(e) => {
                match e.downcast::<FrontendError>() {
                    Ok(FrontendError::UnsupportedFeature(_)) | Ok(FrontendError::TooLarge(_)) => {
                        // Just skip this case.
                        return;
                    }
                    Ok(e) => {
                        panic!("Frontend error: {:?}", e);
                    }
                    Err(e) => {
                        panic!("Other error when parsing module: {:?}", e);
                    }
                }
            }
        };
    parsed_module.expand_all_funcs().unwrap();
    parsed_module.per_func_body(|body| body.optimize(&OptOptions::default()));
    let _ = parsed_module.to_wasm_bytes();
});
