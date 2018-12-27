mod convert;
mod instruction;
mod opcode;
mod parse;
mod util;

use crate::{convert::*, parse::*};
use std::{
    env,
    fs,
    io::{self, Write},
    mem::transmute,
    process::exit,
};

fn main() -> Result<(), io::Error> {
    let args: Vec<String> = env::args().collect();
    let fname = &args[1];

    let mut stdout = io::stdout();

    let s = fs::read_to_string(fname)?;

    let mut err = false;
    for instr in parse(&s) {
        match instr {
            Err(s) => {
                eprintln!("{}", s);
                err = true;
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

    if err {
        exit(1);
    }
    Ok(())
}
