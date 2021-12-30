# WAFFLE: Wasm Analysis Framework for Lightweight Experimentation

Synopsis: an SSA IR compiler framework for Wasm-to-Wasm transforms, in Rust.

## Status: incomplete with known bugs

The transform from Wasm to IR works well, and has been fuzzed.

The transform from IR to Wasm works to roundtrip a simple WASI "hello world"
written in C, but appears to have some bugs still. There are a few fuzzing
oracles (differential execution of roundtripped code, double-roundtripping)
that I have tried to use to shake out bugs, but it's not quite there yet.

Nothing in a middle-end has been designed or written, and the IR traversal APIs
could use work. I'm trying to get roundtripping working with the right basic
abstraction (CFG of SSA) first.

This is a *hobby side project*; please do not expect support or rely on this
for anything critical! I'm happy to accept PRs that make this more
production-ready, of course.

## Architecture

The IR is a CFG of blocks, containing operators that correspond 1-to-1 to Wasm
operators. Dataflow is via SSA, and blocks have blockparams (rather than
phi-nodes). Wasm locals are not used in the IR (they are converted to SSA).

The frontend converts Wasm into this IR by building SSA as it goes, inserting
blockparams when it discovers multiple reaching definitions for a local.
Multivalue Wasm (parameters and results for every control-flow block) is fully
supported, and converted to SSA. This process more or less works like
Cranelift's does, except that memory, table, etc. operations remain at the Wasm
abstraction layer (are not lowered into implementation details), and arithmetic
operators mirror Wasm's exactly.

The backend operates in several stages:

* [Structured control flow recovery](src/backend/structured.rs), which computes
  a loop nest then adds blocks for noncontiguous forward edges, mirroring the
  [Stackifier](https://medium.com/leaningtech/solving-the-structured-control-flow-problem-once-and-for-all-5123117b1ee2)
  algorithm.

* [Serialization](src/backend/serialize.rs), converting the CFG itself into a
  linear sequence of Wasm operators, with resolved block targets and explicit
  use of the stack. References to locals lowered from SSA are still virtual
  (not allocated yet).

* [Location assignment ("regalloc")](src/backend/locations.rs), allocating Wasm
  locals to store values as they flow from operator to operator.

  We use Wasm locals rather than the stack for most dataflow. We can use the
  stack in a limited way, opportunistically, but doing more requires
  sophisticated scheduling (moving operators so they occur at the right time),
  which we haven't yet implemented. 

  We could allocate a local per SSA value, but this is very wasteful. First,
  there is a limit to the number of locals supported by most engines. Second,
  "baseline" compilation strategies typically just allocate a stackslot per
  local, and so this would create huge, sparsely-used frames.

  Instead, we reuse locals by tracking live-ranges of each SSA value and
  assigning non-overlapping ranges to the same local. This is actually a very
  simple register allocator. It remains simpler than other production
  allocators by (i) only tracking a single span for each SSA value, rather than
  a discontiguous set of spans, and (ii) freely reaching for new locals when
  needed rather than "spilling" (it behaves as if the register file is
  infinite).

* [Finalization](src/backend/final.rs), doing a few final rewrite steps to get
  actual Wasm bytecode, produced using
  [wasm-encoder](https://github.com/bytecodealliance/wasm-tools/blob/main/crates/wasm-encoder/README.md).

## Comparisons / Related Work

- Like [Binaryen](https://github.com/WebAssembly/binaryen) but with an SSA IR,
  rather than an AST-based IR. Dataflow analyses are much easier when one
  doesn't have to handle arbitrary reads and writes to locals. Binaryen is able
  to stackify/reloop arbitrary control flow (CFG to Wasm) but does not
  implement the other direction (Wasm to CFG), and it has only a C/C++ API, not
  Rust.

- Like [Walrus](https://github.com/rustwasm/walrus) but also with an SSA IR.
  Walrus is in Rust and designed for Wasm-to-Wasm transforms as well, but its
  IR mirrors the Wasm bytecode closely and thus presents the same difficulties
  as Binaryen for traditional CFG-of-SSA-style compiler analyses and
  transforms.

- Halfway like
  [Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/),
  in that the IR is similar to Cranelift's (a CFG of SSA IR with blockparams),
  but with the Wasm backend as well (Cranelift only does Wasm-to-IR). WAFFLE's
  IR also deliberately remains at the Wasm abstraction level, maintaining
  1-to-1 correspondence with all operators and maintaining the concepts of
  memories, tables, etc., while Cranelift lowers operations and storage
  abstractions into runtime/embedding-specific implementation details in the
  IR.
