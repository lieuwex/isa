#[allow(non_camel_case_types)]
#[derive(Copy, Clone)]
pub enum Configuration {
    imm,
    rd_imm,
    r1_imm,
    rd_r1_r2,
    rd_r1,
    r1_r2,
}

trait OpcodeLike: Sized {
    fn from_str(s: &str) -> Option<Self>;
    fn configuration(self) -> Configuration;
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum Opcode {
    li,   // 0b0000000
    mv,   // 0b0000001
    add,  // 0b0000010
    sub,  // 0b0000011
    mul,  // 0b0000100
    div,  // 0b0000101
    not,  // 0b0000110
    and,  // 0b0000111
    or,   // 0b0001000
    xor,  // 0b0001001
    sll,  // 0b0001010
    slr,  // 0b0001011
    sar,  // 0b0001100
    lt,   // 0b0001101
    lte,  // 0b0001110
    call, // 0b0001111
    jnz,  // 0b0010000
    jz,   // 0b0010001
    jnzr, // 0b0010010
    jzr,  // 0b0010011
    l8,   // 0b0010100
    s8,   // 0b0010101
    l16,  // 0b0010110
    s16,  // 0b0010111
    l32,  // 0b0011000
    s32,  // 0b0011001
    l64,  // 0b0011010
    s64,  // 0b0011011
}

impl OpcodeLike for Opcode {
    fn from_str(s: &str) -> Option<Self> {
        let res = match s.to_lowercase().as_str() {
            "li" => Opcode::li,
            "mv" => Opcode::mv,
            "add" => Opcode::add,
            "sub" => Opcode::sub,
            "mul" => Opcode::mul,
            "div" => Opcode::div,
            "not" => Opcode::not,
            "and" => Opcode::and,
            "or" => Opcode::or,
            "xor" => Opcode::xor,
            "sll" => Opcode::sll,
            "slr" => Opcode::slr,
            "sar" => Opcode::sar,
            "lt" => Opcode::lt,
            "lte" => Opcode::lte,
            "call" => Opcode::call,
            "jnz" => Opcode::jnz,
            "jz" => Opcode::jz,
            "jnzr" => Opcode::jnzr,
            "jzr" => Opcode::jzr,
            "l8" => Opcode::l8,
            "s8" => Opcode::s8,
            "l16" => Opcode::l16,
            "s16" => Opcode::s16,
            "l32" => Opcode::l32,
            "s32" => Opcode::s32,
            "l64" => Opcode::l64,
            "s64" => Opcode::s64,

            _ => return None,
        };
        Some(res)
    }

    fn configuration(self) -> Configuration {
        match self {
            Opcode::add => Configuration::rd_r1_r2,
            Opcode::sub => Configuration::rd_r1_r2,
            Opcode::mul => Configuration::rd_r1_r2,
            Opcode::div => Configuration::rd_r1_r2,
            Opcode::and => Configuration::rd_r1_r2,
            Opcode::or => Configuration::rd_r1_r2,
            Opcode::xor => Configuration::rd_r1_r2,
            Opcode::sll => Configuration::rd_r1_r2,
            Opcode::slr => Configuration::rd_r1_r2,
            Opcode::sar => Configuration::rd_r1_r2,
            Opcode::lt => Configuration::rd_r1_r2,
            Opcode::lte => Configuration::rd_r1_r2,

            Opcode::li => Configuration::rd_imm,
            Opcode::call => Configuration::rd_imm,

            Opcode::jnz => Configuration::r1_imm,
            Opcode::jz => Configuration::r1_imm,

            Opcode::s8 => Configuration::r1_r2,
            Opcode::s16 => Configuration::r1_r2,
            Opcode::s32 => Configuration::r1_r2,
            Opcode::s64 => Configuration::r1_r2,
            Opcode::jnzr => Configuration::r1_r2,
            Opcode::jzr => Configuration::r1_r2,

            Opcode::l8 => Configuration::rd_r1,
            Opcode::l16 => Configuration::rd_r1,
            Opcode::l32 => Configuration::rd_r1,
            Opcode::l64 => Configuration::rd_r1,
            Opcode::not => Configuration::rd_r1,
            Opcode::mv => Configuration::rd_r1,
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PseudoOpcode {
    j,
    gt,
    gte,
}

impl OpcodeLike for PseudoOpcode {
    fn from_str(s: &str) -> Option<Self> {
        let res = match s.to_lowercase().as_str() {
            "j" => PseudoOpcode::j,
            "gt" => PseudoOpcode::gt,
            "gte" => PseudoOpcode::gte,
            _ => return None,
        };
        Some(res)
    }

    fn configuration(self) -> Configuration {
        match self {
            PseudoOpcode::j => Configuration::imm,

            PseudoOpcode::gt => Configuration::rd_r1_r2,
            PseudoOpcode::gte => Configuration::rd_r1_r2,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum InternalOpcode {
    Real(Opcode),
    Pseudo(PseudoOpcode),
}

pub fn str_to_internal_opcode(s: &str) -> Option<InternalOpcode> {
    if let Some(op) = Opcode::from_str(s) {
        Some(InternalOpcode::Real(op))
    } else if let Some(op) = PseudoOpcode::from_str(s) {
        Some(InternalOpcode::Pseudo(op))
    } else {
        None
    }
}

impl InternalOpcode {
    pub fn configuration(self) -> Configuration {
        match self {
            InternalOpcode::Real(x) => x.configuration(),
            InternalOpcode::Pseudo(x) => x.configuration(),
        }
    }
}
