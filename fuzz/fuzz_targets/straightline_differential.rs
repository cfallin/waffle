#![no_main]
use libfuzzer_sys::{arbitrary, fuzz_target};
use std::sync::atomic::{AtomicU64, Ordering};

use waffle::Module;

fn reject(bytes: &[u8]) -> bool {
    let parser = wasmparser::Parser::new(0);
    let mut has_start = false;
    let mut has_global_set = false;
    let mut num_globals = 0;
    for payload in parser.parse_all(bytes) {
        match payload.unwrap() {
            wasmparser::Payload::CodeSectionEntry(body) => {
                for op in body.get_operators_reader().unwrap() {
                    let op = op.unwrap();
                    match op {
                        wasmparser::Operator::Loop { .. } => {
                            // Disallow direct loops.
                            return true;
                        }
                        wasmparser::Operator::Call { .. }
                        | wasmparser::Operator::CallIndirect { .. } => {
                            // Disallow recursion.
                            return true;
                        }
                        wasmparser::Operator::GlobalSet { .. } => {
                            has_global_set = true;
                        }
                        _ => {}
                    }
                }
            }
            wasmparser::Payload::StartSection { .. } => {
                has_start = true;
            }
            wasmparser::Payload::ExportSection(mut reader) => {
                for _ in 0..reader.get_count() {
                    let e = reader.read().unwrap();
                    match &e.kind {
                        &wasmparser::ExternalKind::Global => {
                            num_globals += 1;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    if !has_start || !has_global_set || num_globals < 1 {
        return true;
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
    fn max_memory_pages(&self, _is_64: bool) -> u64 {
        1
    }
}

fuzz_target!(|module: wasm_smith::ConfiguredModule<Config>| {
    let _ = env_logger::try_init();
    log::debug!("original module: {:?}", module.module);

    let orig_bytes = module.module.to_bytes();

    if reject(&orig_bytes[..]) {
        log::debug!("Discarding fuzz run. Body:\n{:?}", module);
        return;
    } else {
        log::info!("body: {:?}", module);
    }

    let engine = wasmtime::Engine::default();
    let mut store = wasmtime::Store::new(&engine, ());
    let orig_module =
        wasmtime::Module::new(&engine, &orig_bytes[..]).expect("failed to parse original wasm");
    let orig_instance = wasmtime::Instance::new(&mut store, &orig_module, &[]);
    let orig_instance = match orig_instance {
        Ok(orig_instance) => orig_instance,
        Err(e) => {
            log::info!("cannot run start on orig intsance ({:?}); discarding", e);
            return;
        }
    };

    let parsed_module = Module::from_wasm_bytes(&orig_bytes[..]).unwrap();
    let roundtrip_bytes = parsed_module.to_wasm_bytes().unwrap();

    if let Ok(filename) = std::env::var("FUZZ_DUMP_WASM") {
        std::fs::write(format!("{}_orig.wasm", filename), &orig_bytes[..]).unwrap();
        std::fs::write(format!("{}_roundtrip.wasm", filename), &roundtrip_bytes[..]).unwrap();
    }

    let total = TOTAL.fetch_add(1, Ordering::Relaxed);

    let roundtrip_module = wasmtime::Module::new(&engine, &roundtrip_bytes[..])
        .expect("failed to parse roundtripped wasm");
    let roundtrip_instance = wasmtime::Instance::new(&mut store, &roundtrip_module, &[])
        .expect("cannot instantiate roundtripped wasm");

    // Ensure exports are equal.

    let a_globals: Vec<_> = orig_instance
        .exports(&mut store)
        .filter_map(|e| e.into_global())
        .collect();
    let a_globals: Vec<wasmtime::Val> = a_globals.into_iter().map(|g| g.get(&mut store)).collect();
    let a_mems: Vec<wasmtime::Memory> = orig_instance
        .exports(&mut store)
        .filter_map(|e| e.into_memory())
        .collect();

    let b_globals: Vec<_> = roundtrip_instance
        .exports(&mut store)
        .filter_map(|e| e.into_global())
        .collect();
    let b_globals: Vec<wasmtime::Val> = b_globals.into_iter().map(|g| g.get(&mut store)).collect();
    let b_mems: Vec<wasmtime::Memory> = roundtrip_instance
        .exports(&mut store)
        .filter_map(|e| e.into_memory())
        .collect();

    log::info!("a_globals = {:?}", a_globals);
    log::info!("b_globals = {:?}", b_globals);

    assert_eq!(a_globals.len(), b_globals.len());
    for (a, b) in a_globals.into_iter().zip(b_globals.into_iter()) {
        match (a, b) {
            (wasmtime::Val::I32(a), wasmtime::Val::I32(b)) => assert_eq!(a, b),
            (wasmtime::Val::I64(a), wasmtime::Val::I64(b)) => assert_eq!(a, b),
            (wasmtime::Val::F32(a), wasmtime::Val::F32(b)) => assert_eq!(a, b),
            (wasmtime::Val::F64(a), wasmtime::Val::F64(b)) => assert_eq!(a, b),
            _ => panic!("mismatching types"),
        }
    }

    assert_eq!(a_mems.len(), b_mems.len());
    for (a, b) in a_mems.into_iter().zip(b_mems.into_iter()) {
        let a_data = a.data(&store);
        let b_data = b.data(&store);
        assert_eq!(a_data, b_data);
    }

    success(total);
});

static TOTAL: AtomicU64 = AtomicU64::new(0);
static SUCCESS: AtomicU64 = AtomicU64::new(0);

fn success(total: u64) {
    let value = SUCCESS.fetch_add(1, Ordering::Relaxed);
    if value % 1000 == 0 {
        eprintln!("SUCCESS: {} / TOTAL: {}", value, total);
    }
}
