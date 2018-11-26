use instruction::*;
use opcode::*;
use regex::{Regex};
use std::collections::HashMap;
use std::fmt;
use util::*;

#[derive(Debug, Clone)]
pub struct ParseError {
    description: String,
    line_number: usize,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "parse error at line {}: {}",
            self.line_number, self.description
        )
    }
}

struct ParseContext {
    labels: HashMap<String, i64>,
    n_instructions: i64,
}

// "tokenize"
struct Tokenizer {
    chars: Vec<char>,
    cursor: usize,
}

fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

impl Tokenizer {
    fn curr(&self) -> char {
        self.chars[self.cursor]
    }
    fn inbound(&self) -> bool {
        self.cursor < self.chars.len()
    }

    fn skip(&mut self, n: usize) {
        self.cursor += n;
    }
    fn skip_spaces(&mut self) {
        while self.inbound() && self.curr().is_whitespace() {
            self.cursor += 1;
        }
    }
    fn get_word(&mut self) -> String {
        let mut res = String::new();
        while self.inbound() && is_word_char(self.curr()) {
            res.push(self.curr());
            self.cursor += 1;
        }
        res
    }

    pub fn tokenize(&mut self) -> (String, Vec<String>) {
        self.skip_spaces();
        let instruction = self.get_word();
        self.skip_spaces();

        let mut args = vec![];
        while self.inbound() {
            self.skip_spaces();

            args.push(self.get_word());
            self.skip_spaces();

            if !self.inbound() || self.curr() != ',' {
                break;
            }
            self.skip(1);
        }

        (instruction, args)
    }

    pub fn new(s: &str) -> Self {
        Self {
            chars: s.chars().collect(),
            cursor: 0,
        }
    }
}

impl ParseContext {
    fn parse_line(&mut self, line: &str, line_number: usize) -> Option<InternalInstruction> {
        lazy_static! {
            static ref commentreg: Regex = Regex::new(r";.+$").unwrap();
            static ref labelreg: Regex = Regex::new(r"(?i)^([a-z]\w*):$").unwrap();
        }

        // remove comments and trim the line, we only need code
        let line = commentreg.replace_all(line, "").into_owned();
        let line = line.trim();

        // skip lines without code
        if line.is_empty() {
            return None;
        }

        // if this is a label declaration line, add the label to the labels map and skip
        // the current line
        match labelreg.captures(line) {
            Some(caps) => {
                let label = &caps[1];
                self.labels.insert(label.to_string(), self.n_instructions);
                return None;
            }
            None => {}
        }

        let (instr, args) = Tokenizer::new(line).tokenize();
        let get_reg_num = |i: usize| {
            let n: String = args[i].chars()
                .skip(1)
                .collect();
            parse_number(n).unwrap() as u8
        };

        // the thing we're going to return, parse the instruction from the line too and
        // convert it to its opcode
        let mut res = InternalInstruction {
            opcode: str_to_internal_opcode(instr.as_str()),
            rd: 0,
            rs1: 0,
            rs2: 0,
            immediate: Immediate::Value(0),
            line_number: line_number,
        };

        let n_instr = self.n_instructions;
        let get_immediate = |n: usize| {
            // the immediate should be a literal value when the first
            // character is a digit, otherwise we treat it as a unevaluated
            // label reference.
            let r2 = args[n].to_string();
            let res = if r2.chars().next()?.is_digit(10) {
                Immediate::Value(parse_number(r2).unwrap())
            } else {
                Immediate::LabelRef(r2.to_string(), n_instr)
            };
            Ok(res)
        };

        // REVIEW: check if non decimal numbers are supported
        // match the parsed opcode to its configuration, then retrieve the correct
        // values for every field
        match res.opcode.configuration() {
            Configuration::imm => {
                res.immediate = get_immediate(0)?;
            }
            Configuration::rd_imm => {
                res.rd = get_reg_num(0);
                res.immediate = get_immediate(1)?;
            }
            Configuration::r1_imm => {
                res.rs1 = get_reg_num(0);
                res.immediate = get_immediate(1)?;
            }
            Configuration::rd_r1_r2 => {
                res.rd = get_reg_num(0);
                res.rs1 = get_reg_num(1);
                res.rs2 = get_reg_num(2);
            }
            Configuration::rd_r1 => {
                res.rd = get_reg_num(0);
                res.rs1 = get_reg_num(1);
            }
            Configuration::r1_r2 => {
                res.rs1 = get_reg_num(0);
                res.rs2 = get_reg_num(1);
            }
        };

        self.n_instructions += 8;
        Some(res)
    }

    fn convert_instruction(
        &self,
        instr: &InternalInstruction,
    ) -> Result<Vec<Instruction>, ParseError> {
        let immediate = match instr.immediate {
            Immediate::LabelRef(ref labelname, labelloc) => {
                match self.labels.get(labelname.as_str()) {
                    Some(instrloc) => {
                        instrloc - labelloc - 8
                    }
                    None => {
                        return Err(ParseError {
                            description: format!("couldn't find label '{}'", labelname),
                            line_number: instr.line_number,
                        });
                    }
                }
            }
            Immediate::Value(val) => val,
        };

        let res = match instr.opcode {
            InternalOpcode::Opaque(OpaqueOpcode::j) => {
                vec![Instruction {
                    opcode: Opcode::jnz,
                    rs1: 0,
                    rs2: 0,
                    rd: 0,
                    immediate: immediate,
                }]
            }

            InternalOpcode::Opaque(OpaqueOpcode::gt) => {
                vec![Instruction {
                    opcode: Opcode::lt,
                    rs1: instr.rs2,
                    rs2: instr.rs1,
                    rd: instr.rd,
                    immediate: 0,
                }]
            }
            InternalOpcode::Opaque(OpaqueOpcode::gte) => {
                vec![Instruction {
                    opcode: Opcode::lte,
                    rs1: instr.rs2,
                    rs2: instr.rs1,
                    rd: instr.rd,
                    immediate: 0,
                }]
            }

            InternalOpcode::Real(opcode) => {
                vec![Instruction {
                    opcode: opcode,
                    rs1: instr.rs1,
                    rs2: instr.rs2,
                    rd: instr.rd,
                    immediate: immediate,
                }]
            }
        };

        Ok(res)
    }
}

pub fn parse(prog: &str) -> Result<Vec<Instruction>, ParseError> {
    let mut ctx = ParseContext {
        labels: HashMap::new(),
        n_instructions: 0,
    };

    let parsed: Vec<InternalInstruction> = prog
        .lines()
        .enumerate()
        .map(|(i, l)| ctx.parse_line(l, i))
        .filter(|x| x.is_some())
        .map(|x| x.unwrap())
        .collect();

    let mut res = vec![];
    for instr in parsed {
        let mut conv = ctx.convert_instruction(&instr)?;
        res.append(&mut conv);
    }
    Ok(res)
}
