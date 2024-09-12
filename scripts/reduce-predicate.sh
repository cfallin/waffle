#!/bin/bash

timeout 1 wasmtime run --disable-cache $1
if [ $? -ne 0 ]; then
    echo bad: initial run crashes too
    exit 1
fi
target/release/waffle-util roundtrip -i $1 -o o.wasm
if [ $? -ne 0 ]; then
    echo bad: roundtrip
    exit 1
fi
wasmtime run --disable-cache o.wasm
if [ $? -ne 0 ]; then
    echo ok: still crashes
    exit 0
else
    echo bad: no longer crashes
    exit 1
fi
