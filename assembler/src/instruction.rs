use opcode::*;

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub opcode: Opcode,
    pub rs1: u8,
    pub rs2: u8,
    pub rd: u8,
    pub immediate: i64,
}

impl Instruction {
    pub fn encode(&self) -> u64 {
        let mut res: u64 = 0;
        res |= self.opcode as u64;
        res |= u64::from(self.rd) << 7;
        res |= u64::from(self.rs1) << 11;
        res |= u64::from(self.rs2) << 15;
        res |= (self.immediate as u64) << 19;
        res
    }
}

#[derive(Debug, Clone)]
pub enum Immediate {
    Value(i64),
    LabelRef(String, i64),
}

#[derive(Debug, Clone)]
pub struct InternalInstruction {
    pub opcode: InternalOpcode,
    pub rs1: u8,
    pub rs2: u8,
    pub rd: u8,
    pub immediate: Immediate,
    pub line_number: usize,
}
