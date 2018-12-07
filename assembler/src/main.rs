mod convert;
mod instruction;
mod opcode;
mod parse;
mod util;

use crate::convert::*;
use crate::parse::*;
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

    for instr in parse(&s) {
        match instr {
            Err(s) => {
                eprintln!("{}", s);
                exit(1);
            }
            Ok(instr) => {
                let instrs = convert_instruction(&instr);
                for instr in instrs {
                    let val = instr.encode();
                    let bytes: [u8; 8] = unsafe { transmute(val.to_le()) };
                    stdout.write_all(&bytes)?;
                }
            }
        }
    }

    Ok(())
}
