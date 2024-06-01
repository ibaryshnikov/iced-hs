#!/bin/bash

# debug build by default
cargo build
cp target/debug/libiced_hs.a .

# to build in release mode use
#cargo build --release
#cp target/release/libiced_hs.a .
