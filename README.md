<div align="center">
  <h1>waffle</h1>

  <p>
    <strong>waffle Wasm compiler library</strong>
  </p>

  <strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <a href="https://github.com/bytecodealliance/waffle/actions?query=workflow%3ACI"><img src="https://github.com/bytecodealliance/waffle/workflows/CI/badge.svg" alt="build status" /></a>
    <a href="https://bytecodealliance.zulipchat.com/#narrow/stream/223391-wasm"><img src="https://img.shields.io/badge/zulip-join_chat-brightgreen.svg" alt="zulip chat" /></a>
    <a href="https://docs.rs/wizer"><img src="https://docs.rs/wizer/badge.svg" alt="Documentation Status" /></a>
  </p>

  <h3>
    <a href="https://docs.rs/wizer">API Docs</a>
    <span> | </span>
    <a href="https://github.com/bytecodealliance/wizer/blob/main/CONTRIBUTING.md">Contributing</a>
    <span> | </span>
    <a href="https://bytecodealliance.zulipchat.com/#narrow/stream/223391-wasm">Chat</a>
  </h3>
</div>

waffle (the Wasm Analysis Framework for Lightweight Experimentation)
is an SSA IR compiler framework for Wasm-to-Wasm transforms, in Rust.

## Status

The transforms from Wasm to IR and from IR to Wasm work well, and has been
fuzzed in various ways. In particular, waffle is fuzzed by roundtripping Wasm
through SSA IR and back, and differentially executing the original and
roundtripped Wasm under Wasmtime (with limits on execution time). At this time,
no correctness bugs are known.

Waffle is able to roundtrip (convert to IR, then compile back to Wasm) complex
modules such as the SpiderMonkey JS engine compiled to Wasm.

Waffle has some basic mid-end optimizations working, such as GVN and constant
propagation. Much more could be done on this.

There are various ways in which the generated Wasm bytecode could be improved;
work is ongoing on this.

waffle is in use by
[weval](https://github.com/bytecodealliance/weval), the WebAssembly
partial evaluator, and was developed for this purpose.

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

The backend operates in four stages:

* [Reducifier](src/backend/reducify.rs), which uses context-sensitive
  block duplication to turn loops with side-entrances (which are
  irreducible) into reducible control flow, suitable for lowering to
  Wasm control-flow primitives.

* [Structured control flow recovery](src/backend/stackify.rs), which uses
  [Ramsey's algorithm](https://dl.acm.org/doi/abs/10.1145/3547621) to convert
  the CFG back into an AST of Wasm control-flow primitives (blocks, loops, and
  if-then AST nodes).

* [Treeification](src/backend/treeify.rs), which computes whether some SSA
  values are used only once and can be moved to just before their single
  consumer, computing the value directly onto the Wasm stack without the need
  for an intermediate local. This is a very simple form of code scheduling.

* [Localification](src/backend/localify.rs), which performs a register
  allocation (using a simple linear-scan algorithm) to assign all SSA values to
  locals such that no live-ranges overlap in the same local.

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
