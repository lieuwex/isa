extern crate regex;
#[macro_use]
extern crate lazy_static;

mod instruction;
mod opcode;
mod parse;
mod util;

use parse::*;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::mem::transmute;
use std::process::exit;

fn main() -> Result<(), io::Error> {
    let args: Vec<String> = env::args().collect();
    let fname = &args[1];

    let mut stdout = io::stdout();

    let s = fs::read_to_string(fname)?;

    match parse(&s) {
        Err(s) => {
            eprintln!("{}", s);
            exit(1);
        }
        Ok(instrs) => {
            for instr in instrs {
                let val = instr.encode();
                let bytes: [u8; 8] = unsafe { transmute(val.to_le()) };
                stdout.write(&bytes)?;
            }
        }
    }

    Ok(())
}
