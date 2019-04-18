#![allow(clippy::unreadable_literal)]
#![warn(clippy::cast_ptr_alignment)]

mod cpu;
mod instruction;
mod memory;
mod opcode;
mod registers;
mod debug;

use crate::{cpu::*,debug::DebugMode};
use std::{env, fs, io, slice};

fn main() -> Result<(), io::Error> {
    let args: Vec<String> = env::args().collect();
    let fname = &args[1];

    let bytes = fs::read(fname)?;

    let prog: &[u64] = unsafe {
        let ptr = bytes.as_ptr() as *const u64;
        slice::from_raw_parts(ptr, bytes.len() / 8)
    };

    let debug_mode = match &env::var_os("DEBUG").and_then(|x| x.into_string().ok()) {
        // rip
        Some(ref s) => match &s[..] {
            "LACKEY" => DebugMode::Lackey,
            "HUMAN" => DebugMode::Human,
            _ => DebugMode::None,
        }

        _ => DebugMode::None,
    };
    let mut cpu = CPU::new(Vec::from(prog), debug_mode);

    if cpu.exec_loop().is_none() {
        eprintln!("error while executing");
    }

    Ok(())
}
