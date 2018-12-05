mod cpu;
mod registers;
mod instruction;
mod memory;
mod opcode;

use cpu::*;
use std::slice;
use std::io::{self, Read};

fn main() -> Result<(), io::Error> {
    let mut bytes: Vec<u8> = vec![];
    io::stdin().read_to_end(&mut bytes)?;

    let prog: &[u64] = unsafe {
        let ptr = bytes.as_ptr() as *const u64;
        slice::from_raw_parts(ptr, bytes.len() / 8)
    };

    let mut cpu = CPU::new(Vec::from(prog));
    match cpu.exec_loop() {
        None => eprintln!("error while executing"),
        _ => {},
    };

    Ok(())
}
