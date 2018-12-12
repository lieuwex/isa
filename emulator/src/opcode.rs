#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
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
    unknown,
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
        0b0001101 => Opcode::lt,
        0b0001110 => Opcode::lte,
        0b0001111 => Opcode::call,
        0b0010000 => Opcode::jnz,
        0b0010001 => Opcode::jz,
        0b0010010 => Opcode::jnzr,
        0b0010011 => Opcode::jzr,
        0b0010100 => Opcode::l8,
        0b0010101 => Opcode::s8,
        0b0010110 => Opcode::l16,
        0b0010111 => Opcode::s16,
        0b0011000 => Opcode::l32,
        0b0011001 => Opcode::s32,
        0b0011010 => Opcode::l64,
        0b0011011 => Opcode::s64,

        _ => Opcode::unknown,
    }
}
