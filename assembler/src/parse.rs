use std::collections::HashMap;
use regex::{self, Regex};
use opcode::*;
use instruction;

#[derive(Debug)]
enum Immediate {
    Value(i64),
    LabelRef(String, i64),
}

#[derive(Debug)]
struct Instruction {
    opcode: Opcode,
    rs1: u8,
    rs2: u8,
    rd: u8,
    immediate: Immediate,
}

struct ParseContext {
    labels: HashMap<String, i64>,
    n_instructions: i64,
}

impl ParseContext {
    fn parse_line(&mut self, line: &str) -> Option<Instruction> {
        // remove comments and trim the line, we only need code
        let commentreg = Regex::new(r";.+$").unwrap();
        let line = commentreg.replace_all(line, "").into_owned();
        let line = line.trim();

        // skip lines without code
        if line.is_empty() {
            return None
        }

        // if this is a label declaration line, add the label to the labels map and skip
        // the current line
        let labelreg = Regex::new(r"^(?P<label>\w+):$").unwrap();
        match labelreg.captures(line) {
            Some(caps) => {
                let label = &caps["label"];
                self.labels.insert(label.to_string(), self.n_instructions);
                return None
            },
            None => {},
        }

        // helper function to retrieve a capture and convert it to a number
        let get_num = |caps: &regex::Captures, cap: &str| {
            let s = &caps[cap];
            s.parse().unwrap()
        };

        // the thing we're going to return, parse the instruction from the line too and
        // convert it to its opcode
        let mut res = Instruction{
            opcode: str_to_opcode(line.split(' ').next().unwrap()),
            rd: 0,
            rs1: 0,
            rs2: 0,
            immediate: Immediate::Value(0),
        };

        // REVIEW: check if non decimal numbers are supported
        // match the parsed opcode to its configuration, then retrieve the correct
        // values for every field
        match opcode_to_configuration(res.opcode) {
            Configuration::rd_imm => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*(?P<r2>\w+)").unwrap();
                let caps = instrreg.captures(line)?;

                res.rd = get_num(&caps, "r1") as u8;

                // the immediate should be a literal value when the first
                // character is a digit, otherwise we treat it as a unevaluated
                // label reference.
                let r2 = caps["r2"].to_string();
                res.immediate =
                    if r2.chars().next().unwrap().is_digit(10) {
                        Immediate::Value(get_num(&caps, "r2"))
                    } else {
                        Immediate::LabelRef(r2, self.n_instructions)
                    };
            },
            Configuration::rd_r1_r2 => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*r(?P<r2>\d{1,2})\s*,\s*r(?P<r3>\d{1,2})").unwrap();
                let caps = instrreg.captures(line)?;

                res.rd = get_num(&caps, "r1") as u8;
                res.rs1 = get_num(&caps, "r2") as u8;
                res.rs2 = get_num(&caps, "r3") as u8;
            },
            Configuration::rd_r1 => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*r(?P<r2>\d{1,2})").unwrap();
                let caps = instrreg.captures(line)?;

                res.rd = get_num(&caps, "r1") as u8;
                res.rs1 = get_num(&caps, "r2") as u8;
            },
            Configuration::r1_r2 => {
                let instrreg = Regex::new(r"\w+\s*r(?P<r1>\d{1,2})\s*,\s*r(?P<r2>\d{1,2})").unwrap();
                let caps = instrreg.captures(line)?;

                res.rs1 = get_num(&caps, "r1") as u8;
                res.rs2 = get_num(&caps, "r2") as u8;
            },
        };

        self.n_instructions += 8;
        Some(res)
    }
}

pub fn parse(prog: &str) -> Vec<instruction::Instruction> {
    let mut ctx = ParseContext{
        labels: HashMap::new(),
        n_instructions: 0,
    };

    let mut parsed = vec![];
    for l in prog.lines() {
        let instr = ctx.parse_line(l);
        if instr.is_some() {
            parsed.push(instr.unwrap());
        }
    }

    let mut res = vec![];
    res.reserve(parsed.len());
    for instr in parsed {
        res.push(instruction::Instruction{
            opcode: instr.opcode,
            rs1: instr.rs1,
            rs2: instr.rs2,
            rd: instr.rd,
            immediate: {
                match instr.immediate {
                    Immediate::LabelRef(s, labelloc) => {
                        let instrloc = ctx.labels.get(s.as_str()).unwrap();
                        instrloc - labelloc
                    },
                    Immediate::Value(val) => val,
                }
            },
        });
    }

    res
}
