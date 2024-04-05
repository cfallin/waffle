#![no_main]
use libfuzzer_sys::fuzz_target;

use waffle::{FrontendOptions, InterpContext, InterpResult, Module};

fuzz_target!(|module: waffle::fuzzing::ArbitraryModule| {
    let module = module.0;
    let _ = env_logger::try_init();
    log::debug!("original module: {:?}", module);

    let orig_bytes = module.to_bytes();

    if waffle::fuzzing::reject(&orig_bytes[..]) {
        log::debug!("Discarding fuzz run. Body:\n{:?}", module);
        return;
    } else {
        log::info!("body: {:?}", module);
    }

    let mut parsed_module =
        Module::from_wasm_bytes(&orig_bytes[..], &FrontendOptions::default()).unwrap();
    parsed_module.expand_all_funcs().unwrap();

    let start = parsed_module.start_func.unwrap();

    let mut orig_ctx = match InterpContext::new(&parsed_module) {
        Ok(ctx) => ctx,
        Err(e) => {
            log::trace!("Rejecting due to instantiation error: {:?}", e);
            return;
        }
    };
    orig_ctx.fuel = 10000;

    match orig_ctx.call(&parsed_module, start, &[]) {
        InterpResult::OutOfFuel => {
            // Silently reject.
            log::trace!("Rejecting due to timeout in orig");
            return;
        }
        InterpResult::Trap(..) => {
            // Silently reject.
            log::trace!("Rejecting due to trap in orig");
            return;
        }
        InterpResult::Ok(_) => {}
        ret => panic!("Bad result: {:?}", ret),
    }

    let mut opt_module = parsed_module.clone();
    opt_module.per_func_body(|body| body.optimize());
    opt_module.per_func_body(|body| body.convert_to_max_ssa(None));

    let mut opt_ctx = InterpContext::new(&opt_module).unwrap();
    // Allow a little leeway for opts to not actually optimize.
    opt_ctx.fuel = 20000;
    opt_ctx.call(&opt_module, start, &[]).ok().unwrap();

    log::trace!(
        "Orig ran in {} fuel; opt ran in {} fuel",
        10000 - orig_ctx.fuel,
        20000 - opt_ctx.fuel
    );

    assert_eq!(orig_ctx.memories, opt_ctx.memories);
    assert_eq!(orig_ctx.globals, opt_ctx.globals);
});
