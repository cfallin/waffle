#!/usr/bin/env bash

set -e

cargo fmt --check
cargo check
cargo +nightly fuzz check
