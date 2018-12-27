#!/usr/bin/env bash

cd ./assembler
cargo build
cd ..

cd ./emulator
cargo build
cd ..

temp_file=$(mktemp)
./assembler/target/debug/assembler $1 >$temp_file \
	&& ./emulator/target/debug/emulator $temp_file
