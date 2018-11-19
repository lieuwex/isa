use std::collections::HashMap;
use regex::Regex;
use opcode::*;
use instruction;

#[derive(Debug)]
enum RegValue {
    Value(u64),
    LabelRef(String, i64),
}

#[derive(Debug)]
struct Instruction {
    opcode: Opcode,
    rs1: u8,
    rs2: u8,
    rd: u8,
    immediate: RegValue,
}

struct ParseContext {
    labels: HashMap<String, i64>,
    n_instructions: i64,
}

impl ParseContext {
    fn parse_line(&mut self, line: &str) -> Option<Instruction> {
        let commentreg = Regex::new(r";.+$").unwrap();
        let line = commentreg.replace_all(line, "").into_owned();
        let line = line.trim();
        if line.is_empty() {
            return None
        }

        let labelreg = Regex::new(r"^(?P<label>\w+):$").unwrap();
        match labelreg.captures(line) {
            Some(caps) => {
                let label = &caps["label"];
                self.labels.insert(label.to_string(), self.n_instructions);
                println!("got label {} at {}", label, self.n_instructions);
                return None
            },
            None => {},
        }

        //let instrreg = Regex::new(r"(?P<instr>\w+)\s+(?P<r1>.+?(\s*,\s*(?P<r2>.+?(\s*,\s*(?P<r3>.+?))?))?)?").unwrap();
        // let caps = instrreg.captures(line)?;
        let mut instr = Instruction{
            opcode: str_to_opcode(line.split(' ').next().unwrap()),
            rd: 0,
            rs1: 0,
            rs2: 0,
            immediate: RegValue::Value(0),
        };

        // TODO: support non decimal numbers
        match opcode_to_configuration(instr.opcode) {
            Configuration::rd_imm => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*(?P<r2>\w+)").unwrap();
                let caps = instrreg.captures(line)?;
                let get_num = |cap: &str| -> u64 {
                    let s = &caps[cap];
                    s.parse().unwrap()
                };

                instr.rd = get_num("r1") as u8;

                let r2 = &caps["r2"].to_string();
                let r2 = r2.clone();
                if r2.chars().next().unwrap().is_digit(10) {
                    instr.immediate = RegValue::Value(get_num("r2"));
                } else {
                    instr.immediate = RegValue::LabelRef(r2, self.n_instructions);
                }
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
        };

        self.n_instructions += 1;
        Some(instr)
    }
}

pub fn parse(prog: &str) -> Vec<instruction::Instruction> {
    let mut ctx = ParseContext{
        labels: HashMap::new(),
        n_instructions: 0,
    };
    let mut tmp = vec![];

    for l in prog.lines() {
        let instr = ctx.parse_line(l);
        if instr.is_some() {
            tmp.push(instr.unwrap());
        }
    }

    let mut res = vec![]; // TODO: optimize
    for instr in tmp {
        let mut immediate: i64 = 0;
        match instr.immediate {
            RegValue::LabelRef(s, labelloc) => {
                let instrloc = ctx.labels.get(s.as_str()).unwrap();
                immediate = instrloc - labelloc;
            },
            _ => {},
        }

        res.push(instruction::Instruction{
            opcode: instr.opcode,
            rs1: instr.rs1,
            rs2: instr.rs2,
            rd: instr.rd,
            immediate: immediate,
        });
    }

    res
}
