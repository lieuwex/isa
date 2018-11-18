use std::collections::HashMap;
use regex::Regex;
use instruction::*;
use opcode::*;

struct ParseContext {
    labels: HashMap<String, i32>,
}

impl ParseContext {
    fn parse_line(&mut self, line: &str) -> Option<Instruction> {
        let commentreg = Regex::new(r";.+$").unwrap();
        let line = commentreg.replace_all(line, "").into_owned();
        let line = line.trim();
        if line.is_empty() {
            return None
        }

        //let instrreg = Regex::new(r"(?P<instr>\w+)\s+(?P<r1>.+?(\s*,\s*(?P<r2>.+?(\s*,\s*(?P<r3>.+?))?))?)?").unwrap();
        // let caps = instrreg.captures(line)?;
        let mut instr = Instruction{
            opcode: str_to_opcode(line.split(' ').next().unwrap()),
            rd: 0,
            rs1: 0,
            rs2: 0,
            immediate: 0,
        };

        // TODO: support non decimal numbers
        match opcode_to_configuration(instr.opcode) {
            Configuration::rd_imm => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*(?P<r2>\d+)").unwrap();
                let caps = instrreg.captures(line)?;
                let get_num = |cap: &str| -> u64 {
                    let s = &caps[cap];
                    s.parse().unwrap()
                };

                instr.rd = get_num("r1") as u8;
                instr.immediate = get_num("r2");
            },
            Configuration::rd_r1_r2 => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*r(?P<r2>\d{1,2})\s*,\s*r(?P<r3>\d{1,2})").unwrap();
                let caps = instrreg.captures(line)?;
                let get_num = |cap: &str| -> u64 {
                    let s = &caps[cap];
                    s.parse().unwrap()
                };

                instr.rd = get_num("r1") as u8;
                instr.rs1 = get_num("r2") as u8;
                instr.rs2 = get_num("r3") as u8;
            },
            Configuration::rd_r1 => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*r(?P<r2>\d{1,2})").unwrap();
                let caps = instrreg.captures(line)?;
                let get_num = |cap: &str| -> u64 {
                    let s = &caps[cap];
                    s.parse().unwrap()
                };

                instr.rd = get_num("r1") as u8;
                instr.rs1 = get_num("r2") as u8;
            },
            Configuration::r1_r2 => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*r(?P<r2>\d{1,2})").unwrap();
                let caps = instrreg.captures(line)?;
                let get_num = |cap: &str| -> u64 {
                    let s = &caps[cap];
                    s.parse().unwrap()
                };

                instr.rs1 = get_num("r1") as u8;
                instr.rs2 = get_num("r2") as u8;
            },
        }

        Some(instr)
    }
}

pub fn parse(prog: &str) -> Vec<Instruction> {
    let mut res = vec![];
    let mut ctx = ParseContext{
        labels: HashMap::new(),
    };

    for l in prog.lines() {
        let instr = ctx.parse_line(l);
        if instr.is_some() {
            res.push(instr.unwrap());
        }
    }

    res
}
