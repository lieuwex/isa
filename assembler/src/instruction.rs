use opcode::*;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub rs1: u8,
    pub rs2: u8,
    pub rd: u8,
    pub immediate: u64,
}

pub fn instruction_decode(instruction: u64) -> Instruction {
    Instruction {
        opcode: u8_to_opcode((instruction & 0x7f) as u8),
        rs1: (instruction >> 7 & 0xf) as u8,
        rs2: (instruction >> 11 & 0xf) as u8,
        rd: (instruction >> 15 & 0xf) as u8,
        immediate: (instruction >> 19 & 0x1fffffffffff) as u64,
    }
}

impl Instruction {
    pub fn encode(&self) -> u64 {
        let mut res: u64 = 0;
        res |= self.opcode as u64;
        res |= (self.rs1 as u64) << 7;
        res |= (self.rs2 as u64) << 11;
        res |= (self.rd as u64) << 15;
        res |= self.immediate << 19;
        res
    }
}
