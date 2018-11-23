#!/usr/bin/env sh

cd ./assembler
cargo build
cd ..

cd ./emulator
cargo build
cd ..

./assembler/target/debug/assembler $1 | ./emulator/target/debug/emulator
