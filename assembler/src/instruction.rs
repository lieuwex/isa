use opcode::*;

#[derive(Debug)]
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
        res |= (self.rd as u64) << 7;
        res |= (self.rs1 as u64) << 11;
        res |= (self.rs2 as u64) << 15;
        res |= (self.immediate as u64) << 19;
        res
    }
}
