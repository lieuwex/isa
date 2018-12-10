#!/usr/bin/env bash

cd ./assembler
cargo build
cd ..

cd ./emulator
cargo build
cd ..

./emulator/target/debug/emulator <(./assembler/target/debug/assembler $1)
