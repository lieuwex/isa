extern crate regex;

mod opcode;
mod instruction;
mod parse;

use std::env;
use std::fs;
use std::io::{self, Write};
use parse::*;
use std::mem::transmute;

fn main() {
    let args: Vec<String> = env::args().collect();
    let fname = &args[1];

    let mut stdout = io::stdout();

    let s = fs::read_to_string(fname).unwrap();
    let instrs = parse(&s);
    for instr in instrs {
        let val = instr.encode();
        let bytes: [u8; 8] = unsafe { transmute(val.to_be()) };
        stdout.write(&bytes);
    }
}
