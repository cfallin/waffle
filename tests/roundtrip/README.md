This directory contains Wasm modules (in text format as `.wat` files) that we
roundtrip through waffle. We are testing that we can roundtrip (e.g., that
we support all the included features), and that roundtripping twice gets us the
same module (i.e., that it is idempotent after one "normalization"), but not
that it is comprehensively correct for all of Wasm. Fuzzing (including the
differential-execution targets `differential` and `opt_diff`) should be
considered the more authoritative source of robust roundtripping correctness.
