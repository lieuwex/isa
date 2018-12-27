use crate::{instruction::*, opcode::*, util::*};
use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone)]
pub struct ParseError {
    description: String,
    line_number: usize,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "parse error at line {}: {}",
            self.line_number + 1,
            self.description
        )
    }
}

fn err_int_parse(line_number: usize) -> ParseError {
    ParseError {
        description: String::from("couldn't parse number"),
        line_number,
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
    c.is_alphanumeric() || c == '_' || c == '-'
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

    pub fn tokenize(&mut self, line_number: usize) -> Result<(String, Vec<String>), ParseError> {
        macro_rules! non_empty {
            ($w:expr, $e:expr) => {
                if $w.is_empty() {
                    return Err(ParseError {
                        description: String::from($e),
                        line_number,
                    });
                }
            };
        }

        self.skip_spaces();
        let instruction = self.get_word();
        non_empty!(instruction, "expected instruction");
        self.skip_spaces();

        let mut args = vec![];
        while self.inbound() {
            self.skip_spaces();

            let arg = self.get_word();
            non_empty!(arg, "expected argument");
            args.push(arg);
            self.skip_spaces();

            if !self.inbound() || self.curr() != ',' {
                break;
            }
            self.skip(1);
        }

        Ok((instruction, args))
    }

    pub fn new(s: &str) -> Self {
        Self {
            chars: s.chars().collect(),
            cursor: 0,
        }
    }
}

impl ParseContext {
    fn parse_line(
        &mut self,
        line: &str,
        line_number: usize,
    ) -> Option<Result<InternalInstruction, ParseError>> {
        lazy_static! {
            static ref commentreg: Regex = Regex::new(r";.+$").unwrap();
            static ref labelreg: Regex = Regex::new(r"(?i)^([a-z_]\w*):$").unwrap();
        }

        // remove comments and trim the line, we only need code
        let line = commentreg.replace_all(line, "");
        let line = line.trim();

        // skip lines without code
        if line.is_empty() {
            return None;
        }

        // if this is a label declaration line, add the label to the labels map and skip
        // the current line
        if let Some(caps) = labelreg.captures(line) {
            self.labels.insert(caps[1].to_string(), self.n_instructions);
            return None;
        }

        let (instr, args) = match Tokenizer::new(line).tokenize(line_number) {
            Ok(res) => res,
            Err(err) => return Some(Err(err)),
        };
        macro_rules! assert_len {
            ($n:expr) => {
                if args.len() != $n {
                    return Some(Err(ParseError {
                        description: format!("expected {} arguments, got {}", $n, args.len()),
                        line_number,
                    }));
                }
            };
        }

        let get_reg_num = |i: usize| -> Result<u8, ParseError> {
            let arg = &args[i];
            if !arg.starts_with('r') && !arg.starts_with('R') {
                return Err(ParseError {
                    description: String::from("expected register, got immediate value"),
                    line_number,
                });
            }

            match parse_number(&arg[1..]) {
                Ok(n) => Ok(n as u8),
                Err(_) => Err(err_int_parse(line_number)),
            }
        };
        macro_rules! get_reg {
            ($e:expr) => {
                match get_reg_num($e) {
                    Ok(x) => x,
                    Err(e) => return Some(Err(e)),
                }
            };
        }

        let n_instr = self.n_instructions;
        let get_immediate = |n: usize| -> Result<Immediate, ParseError> {
            // the immediate should be a literal value when the first
            // character is a digit, otherwise we treat it as a unevaluated
            // label reference.
            let r2 = &args[n];

            let c = match r2.chars().next() {
                None => {
                    return Err(ParseError {
                        description: String::from("no immediate value given"),
                        line_number,
                    });
                }
                Some(c) => c,
            };

            let res = if c.is_digit(10) || c == '-' {
                let n = match parse_number(r2) {
                    Ok(n) => n,
                    Err(_) => return Err(err_int_parse(line_number)),
                };
                Immediate::Value(n)
            } else {
                Immediate::LabelRef(r2.to_string(), n_instr)
            };
            Ok(res)
        };
        macro_rules! get_imm {
            ($e:expr) => {
                match get_immediate($e) {
                    Ok(x) => x,
                    Err(e) => return Some(Err(e)),
                }
            };
        }

        let opcode = match str_to_internal_opcode(instr.as_str()) {
            None => {
                let description = format!("unkown instruction {}", instr);
                return Some(Err(ParseError {
                    description: String::from(description),
                    line_number,
                }))
            }
            Some(op) => op,
        };

        // the thing we're going to return, parse the instruction from the line too and
        // convert it to its opcode
        let mut res = InternalInstruction {
            opcode,
            rd: 0,
            rs1: 0,
            rs2: 0,
            immediate: Immediate::Value(0),
            line_number,
        };

        macro_rules! map {
            (@immediate: $val:expr) => {
                res.immediate = get_imm!($val);
            };
            (@$name:ident: $val:expr) => {
                res.$name = get_reg!($val);
            };
            ($( $name:ident: $val:expr ),*) => {
                {
                    let mut n = 0;
                    $(
                        res.$name;
                        n += 1;
                    )*
                    assert_len!(n);

                    $(map!(@$name: $val);)*
                }
            };
        }

        // REVIEW: check if non decimal numbers are supported
        // match the parsed opcode to its configuration, then retrieve the correct
        // values for every field
        match res.opcode.configuration() {
            Configuration::void => map!(),
            Configuration::imm => map!(immediate: 0),
            Configuration::rd_imm => map!(rd: 0, immediate: 1),
            Configuration::r1_imm => map!(rs1: 0, immediate: 1),
            Configuration::rd_r1_r2 => map!(rd: 0, rs1: 1, rs2: 2),
            Configuration::rd_r1 => map!(rd: 0, rs1: 1),
            Configuration::r1_r2 => map!(rs1: 0, rs2: 1),
        };

        self.n_instructions += 8;
        Some(Ok(res))
    }

    fn inline_labels(
        &self,
        mut instr: InternalInstruction,
    ) -> Result<InternalInstruction, ParseError> {
        let val = match instr.immediate {
            Immediate::LabelRef(ref labelname, labelloc) => {
                match self.labels.get(labelname.as_str()) {
                    Some(instrloc) => instrloc - labelloc - 8,
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
        instr.immediate = Immediate::Value(val);
        Ok(instr)
    }
}

pub fn parse(prog: &str) -> Vec<Result<InternalInstruction, ParseError>> {
    let mut ctx = ParseContext {
        labels: HashMap::new(),
        n_instructions: 0,
    };

    let parsed: Vec<Result<InternalInstruction, ParseError>> = prog
        .lines()
        .enumerate()
        .map(|(i, l)| ctx.parse_line(l, i))
        .filter_map(|x| x)
        .collect();

    parsed
        .iter()
        .map(|instr| match instr {
            Ok(x) => ctx.inline_labels(x.clone()),
            Err(e) => Err(e.clone()),
        })
        .collect()
}
