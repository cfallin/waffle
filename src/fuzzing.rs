//! Fuzzing-specific utilities.

// use libfuzzer_sys::arbitrary;

// pub fn reject(bytes: &[u8]) -> bool {
//     let parser = wasmparser::Parser::new(0);
//     let mut has_start = false;
//     let mut has_global_set = false;
//     let mut num_globals = 0;
//     for payload in parser.parse_all(bytes) {
//         match payload.unwrap() {
//             wasmparser::Payload::CodeSectionEntry(body) => {
//                 for op in body.get_operators_reader().unwrap() {
//                     let op = op.unwrap();
//                     match op {
//                         wasmparser::Operator::GlobalSet { .. } => {
//                             has_global_set = true;
//                         }
//                         _ => {}
//                     }
//                 }
//             }
//             wasmparser::Payload::StartSection { .. } => {
//                 has_start = true;
//             }
//             wasmparser::Payload::ExportSection(mut reader) => {
//                 for _ in 0..reader.get_count() {
//                     let e = reader.read().unwrap();
//                     match &e.kind {
//                         &wasmparser::ExternalKind::Global => {
//                             num_globals += 1;
//                         }
//                         _ => {}
//                     }
//                 }
//             }
//             wasmparser::Payload::MemorySection(mut reader) => {
//                 for _ in 0..reader.get_count() {
//                     let m = reader.read().unwrap();
//                     if m.maximum.is_none() || m.maximum.unwrap() > 100 {
//                         return true;
//                     }
//                 }
//             }
//             _ => {}
//         }
//     }

//     if !has_start || !has_global_set || num_globals < 1 {
//         return true;
//     }

//     false
// }

// #[derive(Debug)]
// pub struct Config;

// impl<'a> arbitrary::Arbitrary<'a> for Config {
//     fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
//         Ok(Config)
//     }
// }

// impl wasm_smith::Config for Config {
//     fn min_funcs(&self) -> usize {
//         1
//     }
//     fn max_funcs(&self) -> usize {
//         1
//     }
//     fn min_memories(&self) -> u32 {
//         1
//     }
//     fn max_memories(&self) -> usize {
//         1
//     }
//     fn min_globals(&self) -> usize {
//         10
//     }
//     fn max_globals(&self) -> usize {
//         10
//     }
//     fn min_tables(&self) -> u32 {
//         0
//     }
//     fn max_tables(&self) -> usize {
//         0
//     }
//     fn min_imports(&self) -> usize {
//         0
//     }
//     fn max_imports(&self) -> usize {
//         0
//     }
//     fn min_exports(&self) -> usize {
//         12
//     }
//     fn max_exports(&self) -> usize {
//         12
//     }
//     fn allow_start_export(&self) -> bool {
//         true
//     }
//     fn canonicalize_nans(&self) -> bool {
//         true
//     }
//     fn max_memory_pages(&self, _is_64: bool) -> u64 {
//         1
//     }
// }
