extern crate regex;

mod instruction;
mod opcode;
mod parse;

use parse::*;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::mem::transmute;
use std::process::exit;

fn main() {
    let args: Vec<String> = env::args().collect();
    let fname = &args[1];

    let mut stdout = io::stdout();

    let s = fs::read_to_string(fname).unwrap();
    let instrs = parse(&s);

    let err = false;
    for instr in instrs {
        match instr {
            Ok(instr) => {
                let val = instr.encode();
                let bytes: [u8; 8] = unsafe { transmute(val.to_be()) };
                stdout.write(&bytes).unwrap();
            }
            Err(_) => {
                eprintln!("error while parsing instruction");
            }
        }
    }

    if err {
        exit(1);
    }
}
