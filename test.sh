wat2wasm --enable-tail-call $1 -o $1.wasm
cargo run roundtrip -i $1.wasm -o $1.rt.wasm
wasm2wat --enable-tail-call $1.rt.wasm