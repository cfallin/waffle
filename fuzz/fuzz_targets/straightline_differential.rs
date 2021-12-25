#![no_main]
use libfuzzer_sys::{arbitrary, fuzz_target};

use waffle::Module;

fn has_loop(bytes: &[u8]) -> bool {
    let parser = wasmparser::Parser::new(0);
    for payload in parser.parse_all(bytes) {
        match payload.unwrap() {
            wasmparser::Payload::CodeSectionEntry(body) => {
                for op in body.get_operators_reader().unwrap() {
                    let op = op.unwrap();
                    match op {
                        wasmparser::Operator::Loop { .. } => {
                            return true;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    false
}

#[derive(Debug)]
struct Config;

impl<'a> arbitrary::Arbitrary<'a> for Config {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Config)
    }
}

impl wasm_smith::Config for Config {
    fn min_funcs(&self) -> usize {
        1
    }
    fn max_funcs(&self) -> usize {
        1
    }
    fn min_memories(&self) -> u32 {
        1
    }
    fn max_memories(&self) -> usize {
        1
    }
    fn min_globals(&self) -> usize {
        10
    }
    fn max_globals(&self) -> usize {
        10
    }
    fn min_tables(&self) -> u32 {
        0
    }
    fn max_tables(&self) -> usize {
        0
    }
    fn min_imports(&self) -> usize {
        0
    }
    fn max_imports(&self) -> usize {
        0
    }
    fn min_exports(&self) -> usize {
        12
    }
    fn max_exports(&self) -> usize {
        12
    }
    fn allow_start_export(&self) -> bool {
        true
    }
    fn canonicalize_nans(&self) -> bool {
        true
    }
}

fuzz_target!(|module: wasm_smith::ConfiguredModule<Config>| {
    let _ = env_logger::try_init();
    log::debug!("original module: {:?}", module.module);

    let orig_bytes = module.module.to_bytes();

    if has_loop(&orig_bytes[..]) {
        log::debug!("has a loop; discarding fuzz run");
        return;
    }

    let orig_wasmi_module =
        wasmi::Module::from_buffer(&orig_bytes[..]).expect("failed to parse original wasm");
    let orig_instance =
        wasmi::ModuleInstance::new(&orig_wasmi_module, &wasmi::ImportsBuilder::default())
            .expect("cannot instantiate original wasm")
            .run_start(&mut wasmi::NopExternals);
    let orig_instance = match orig_instance {
        Ok(orig_instance) => orig_instance,
        Err(e) => {
            log::debug!("cannot run start on orig intsance ({:?}); discarding", e);
            return;
        }
    };

    let parsed_module = Module::from_wasm_bytes(&orig_bytes[..]).unwrap();
    let roundtrip_bytes = parsed_module.to_wasm_bytes();

    let roundtrip_wasmi_module =
        wasmi::Module::from_buffer(&roundtrip_bytes).expect("failed to parse roundtripped wasm");
    let roundtrip_instance =
        wasmi::ModuleInstance::new(&roundtrip_wasmi_module, &wasmi::ImportsBuilder::default())
            .expect("cannot instantiate roundtripped wasm")
            .run_start(&mut wasmi::NopExternals)
            .expect("cannot run start on original wasm");

    // Ensure globals are equal.
    assert_eq!(
        orig_instance.globals().len(),
        roundtrip_instance.globals().len()
    );
    for (a, b) in orig_instance
        .globals()
        .iter()
        .zip(roundtrip_instance.globals().iter())
    {
        match (a.get(), b.get()) {
            (wasmi::RuntimeValue::I32(a), wasmi::RuntimeValue::I32(b)) => assert_eq!(a, b),
            (wasmi::RuntimeValue::I64(a), wasmi::RuntimeValue::I64(b)) => assert_eq!(a, b),
            (wasmi::RuntimeValue::F32(a), wasmi::RuntimeValue::F32(b)) => {
                assert_eq!(a.to_bits(), b.to_bits())
            }
            (wasmi::RuntimeValue::F64(a), wasmi::RuntimeValue::F64(b)) => {
                assert_eq!(a.to_bits(), b.to_bits())
            }
            _ => panic!("mismatched types"),
        }
    }
});
