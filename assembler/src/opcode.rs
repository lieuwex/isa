#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum Opcode {
    unknown,
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
    call, // 0b0001101
    jnz,  // 0b0001110
    jz,   // 0b0001111
    jnzr, // 0b0010000
    jzr,  // 0b0010001
    l8,   // 0b0010010
    s8,   // 0b0010011
    l16,  // 0b0010100
    s16,  // 0b0010101
    l32,  // 0b0010110
    s32,  // 0b0010111
    l64,  // 0b0011000
    s64,  // 0b0011001
}

pub fn u8_to_opcode(opcode: u8) -> Opcode {
    match opcode {
        0b0000000 => Opcode::li,
        0b0000001 => Opcode::mv,
        0b0000010 => Opcode::add,
        0b0000011 => Opcode::sub,
        0b0000100 => Opcode::mul,
        0b0000101 => Opcode::div,
        0b0000110 => Opcode::not,
        0b0000111 => Opcode::and,
        0b0001000 => Opcode::or,
        0b0001001 => Opcode::xor,
        0b0001010 => Opcode::sll,
        0b0001011 => Opcode::slr,
        0b0001100 => Opcode::sar,
        0b0001101 => Opcode::call,
        0b0001110 => Opcode::jnz,
        0b0001111 => Opcode::jz,
        0b0010000 => Opcode::jnzr,
        0b0010001 => Opcode::jzr,
        0b0010010 => Opcode::l8,
        0b0010011 => Opcode::s8,
        0b0010100 => Opcode::l16,
        0b0010101 => Opcode::s16,
        0b0010110 => Opcode::l32,
        0b0010111 => Opcode::s32,
        0b0011000 => Opcode::l64,
        0b0011001 => Opcode::s64,

        _ => Opcode::unknown,
    }
}

pub fn str_to_opcode(s: &str) -> Opcode {
    match s.to_lowercase().as_str() {
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

        _ => Opcode::unknown,
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone)]
pub enum Configuration {
    rd_imm,
    rd_r1_r2,
    rd_r1,
    r1_r2,
}

pub fn opcode_to_configuration(opcode: Opcode) -> Configuration {
    match opcode {
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

        Opcode::li => Configuration::rd_imm,
        Opcode::call => Configuration::rd_imm,
        Opcode::jnz => Configuration::rd_imm,
        Opcode::jz => Configuration::rd_imm,

        Opcode::s8 => Configuration::r1_r2,
        Opcode::s16 => Configuration::r1_r2,
        Opcode::s32 => Configuration::r1_r2,
        Opcode::s64 => Configuration::r1_r2,

        Opcode::l8 => Configuration::rd_r1,
        Opcode::l16 => Configuration::rd_r1,
        Opcode::l32 => Configuration::rd_r1,
        Opcode::l64 => Configuration::rd_r1,
        Opcode::not => Configuration::rd_r1,
        Opcode::jnzr => Configuration::rd_r1,
        Opcode::jzr => Configuration::rd_r1,
        Opcode::mv => Configuration::rd_r1,

        // REVIEW
        Opcode::unknown => Configuration::r1_r2,
    }
}
