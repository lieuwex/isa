use instruction;
use std::fmt;
use std::cell::Cell;
use opcode::*;
use regex::{self, Regex};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ParseError {
    description: String,
    line_number: usize,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parse error at line {}: {}", self.line_number, self.description)
    }
}

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
    line_number: usize,
}

struct ParseContext {
    labels: HashMap<String, i64>,
    n_instructions: i64,
}

// "tokenize"
fn tokenize_line(l: &str) -> (String, Vec<String>) {
    let cursor = Cell::new(0);
    let chars: Vec<char> = l.chars().collect();

    let skip_spaces = || {
        let mut c = cursor.get();
        while c < chars.len() && chars[c].is_whitespace() {
            c += 1;
        }
        cursor.set(c);
    };
    let get_word = || {
        let mut res = String::new();
        let mut c = cursor.get();

        while c < chars.len() && chars[c].is_alphanumeric() {
            res.push(chars[c]);
            c += 1;
        }

        cursor.set(c);
        res
    };
    let skip = |n| {
        cursor.set(cursor.get() + n);
    };

    skip_spaces();
    let instruction = get_word();
    skip_spaces();

    let mut args = vec![];
    while cursor.get() < chars.len() {
        skip_spaces();

        if chars[cursor.get()] == 'r' {
            skip(1);
        }

        args.push(get_word());
        skip_spaces();

        if cursor.get() >= chars.len() || chars[cursor.get()] != ',' {
            break;
        }
        skip(1);
    }

    (instruction, args)
}

impl ParseContext {
    fn parse_line(&mut self, line: &str, line_number: usize) -> Option<Instruction> {
        // remove comments and trim the line, we only need code
        let commentreg = Regex::new(r";.+$").unwrap();
        let line = commentreg.replace_all(line, "").into_owned();
        let line = line.trim();

        // skip lines without code
        if line.is_empty() {
            return None;
        }

        // if this is a label declaration line, add the label to the labels map and skip
        // the current line
        let labelreg = Regex::new(r"^(?P<label>\w+):$").unwrap();
        match labelreg.captures(line) {
            Some(caps) => {
                let label = &caps["label"];
                self.labels.insert(label.to_string(), self.n_instructions);
                return None;
            }
            None => {}
        }

        let (instr, args) = tokenize_line(line);

        // the thing we're going to return, parse the instruction from the line too and
        // convert it to its opcode
        let mut res = Instruction {
            opcode: str_to_opcode(instr.as_str()),
            rd: 0,
            rs1: 0,
            rs2: 0,
            immediate: Immediate::Value(0),
            line_number: line_number,
        };

        // REVIEW: check if non decimal numbers are supported
        // match the parsed opcode to its configuration, then retrieve the correct
        // values for every field
        match opcode_to_configuration(res.opcode) {
            Configuration::rd_imm => {
                res.rd = args[0].parse().unwrap();

                // the immediate should be a literal value when the first
                // character is a digit, otherwise we treat it as a unevaluated
                // label reference.
                let r2 = &args[1];
                res.immediate = if r2.chars().next()?.is_digit(10) {
                    Immediate::Value(r2.parse().unwrap())
                } else {
                    Immediate::LabelRef(r2.to_string(), self.n_instructions)
                };
            }
            Configuration::rd_r1_r2 => {
                res.rd = args[0].parse().unwrap();
                res.rs1 = args[1].parse().unwrap();
                res.rs2 = args[2].parse().unwrap();
            }
            Configuration::rd_r1 => {
                res.rd = args[0].parse().unwrap();
                res.rs1 = args[1].parse().unwrap();
            }
            Configuration::r1_r2 => {
                res.rs1 = args[0].parse().unwrap();
                res.rs2 = args[1].parse().unwrap();
            }
        };

        self.n_instructions += 8;
        Some(res)
    }

    fn convert_instruction(
        &self,
        instr: &Instruction,
    ) -> Result<instruction::Instruction, ParseError> {
        let immediate;
        match instr.immediate {
            Immediate::LabelRef(ref labelname, labelloc) => {
                match self.labels.get(labelname.as_str()) {
                    Some(instrloc) => {
                        immediate = instrloc - labelloc;
                    }
                    None => {
                        return Err(ParseError {
                            description: format!("couldn't find label '{}'", labelname),
                            line_number: instr.line_number
                        });
                    }
                }
            }
            Immediate::Value(val) => {
                immediate = val;
            }
        }

        Ok(instruction::Instruction {
            opcode: instr.opcode,
            rs1: instr.rs1,
            rs2: instr.rs2,
            rd: instr.rd,
            immediate: immediate,
        })
    }
}

pub fn parse(prog: &str) -> Vec<Result<instruction::Instruction, ParseError>> {
    let mut ctx = ParseContext {
        labels: HashMap::new(),
        n_instructions: 0,
    };

    let parsed: Vec<Instruction> = prog
        .lines()
        .enumerate()
        .map(|(i, l)| ctx.parse_line(l, i))
        .filter(|x| x.is_some())
        .map(|x| x.unwrap())
        .collect();

    parsed
        .iter()
        .map(|instr| ctx.convert_instruction(&instr))
        .collect()
}
