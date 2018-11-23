use opcode::*;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub rs1: u8,
    pub rs2: u8,
    pub rd: u8,
    pub immediate: i64,
}

pub fn instruction_decode(instruction: u64) -> Instruction {
    Instruction {
        opcode: u8_to_opcode((instruction & 0x7f) as u8 - 1),
        rd: (instruction >> 7 & 0xf) as u8,
        rs1: (instruction >> 11 & 0xf) as u8,
        rs2: (instruction >> 15 & 0xf) as u8,
        immediate: (instruction as i64) >> 19,
    }
}
