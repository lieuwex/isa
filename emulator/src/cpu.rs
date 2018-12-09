use crate::registers::Registers;
use crate::instruction::*;
use crate::memory::Memory;
use crate::opcode::Opcode;
use std::mem::size_of_val;

static END_MARKER: u64 = 0xCAFEBAAAABBEEEEE;

fn sign_extend(val: u64, nbits: u32) -> u64 {
    let x = size_of_val(&val) as u32 * 8 - nbits;
    val.wrapping_shl(x).wrapping_shr(x)
}

pub struct CPU {
    regs: Registers,
    mem: Memory,
}

impl CPU {
    pub fn get_reg(&self, i: usize) -> u64 {
        self.regs.get(i)
    }
    pub fn get_pc(&self) -> usize {
        self.get_reg(0) as usize
    }
    pub fn get_instruction(&self) -> Option<Instruction> {
        let pc = self.get_pc();
        let raw: u64 = self.mem.read_data(pc)?;
        Some(instruction_decode(raw))
    }

    pub fn set_reg(&mut self, i: usize, val: u64) {
        self.regs.set(i, val);
    }
    pub fn inc_pc(&mut self) {
        let pc = self.get_pc() as u64;
        self.set_reg(0, pc + 8);
    }

    pub fn arithmatic(&mut self, instr: &Instruction) -> bool {
        let a: i64 = self.get_reg(instr.rs1 as usize) as i64;
        let b: i64 = self.get_reg(instr.rs2 as usize) as i64;

        let res: i64 = match instr.opcode {
            Opcode::not => !a,
            Opcode::add => a + b,
            Opcode::sub => a - b,
            Opcode::mul => a * b,
            Opcode::div => a / b,
            Opcode::and => a & b,
            Opcode::or => a | b,
            Opcode::xor => a ^ b,
            Opcode::sll => a.wrapping_shl(b as u32),
            Opcode::slr => (a as u64).wrapping_shr(b as u32) as i64,
            Opcode::sar => a.wrapping_shr(b as u32),
            Opcode::lt => (a < b) as i64,
            Opcode::lte => (a <= b) as i64,

            _ => return false,
        };

        self.set_reg(instr.rd as usize, res as u64);
        true
    }
    pub fn jumps(&mut self, instr: &Instruction) -> bool {
        let cond = self.regs.get(instr.rs1 as usize);

        let (usereg, matches) = match instr.opcode {
            Opcode::call => {
                let pc = self.get_pc();
                self.regs.set(instr.rd as usize, pc as u64);
                (false, true)
            }
            Opcode::jnz => (false, cond != 0),
            Opcode::jnzr => (true, cond != 0),
            Opcode::jz => (false, cond == 0),
            Opcode::jzr => (true, cond == 0),

            _ => return false,
        };

        if matches {
            let offset = if usereg {
                self.regs.get(instr.rs2 as usize) as i64
            } else {
                instr.immediate as i64
            };
            let dest = self.get_pc() as i64 + offset;
            self.regs.set(0, dest as u64);
        }

        true
    }
    pub fn loads(&mut self, instr: &Instruction) -> bool {
        let loc: usize = self.regs.get(instr.rs1 as usize) as usize;

        self.regs.set(instr.rd as usize, match instr.opcode {
            Opcode::li => instr.immediate as u64,

            Opcode::l8 => {
                let raw: u8 = self.mem.read_data(loc).unwrap();
                sign_extend(u64::from(raw), 8)
            }
            Opcode::l16 => {
                let raw: u16 = self.mem.read_data(loc).unwrap();
                sign_extend(u64::from(raw), 16)
            },
            Opcode::l32 => {
                let raw: u32 = self.mem.read_data(loc).unwrap();
                sign_extend(u64::from(raw), 32)
            },
            Opcode::l64 => {
                let raw: u64 = self.mem.read_data(loc).unwrap();
                sign_extend(raw, 64)
            },

            _ => return false,
        });

        true
    }
    pub fn stores(&mut self, instr: &Instruction) -> bool {
        let addr = self.regs.get(instr.rs1 as usize) as usize;
        let val = self.regs.get(instr.rs2 as usize);

        match instr.opcode {
            Opcode::s8 => self.mem.write_data(addr, val as u8),
            Opcode::s16 => self.mem.write_data(addr, val as u16),
            Opcode::s32 => self.mem.write_data(addr, val as u32),
            Opcode::s64 => self.mem.write_data(addr, val as u64),

            _ => return false,
        };

        true
    }

    pub fn execute(&mut self, instr: Instruction) {
        let mut done = false;
        if !done { done = self.arithmatic(&instr); }
        if !done { done = self.jumps(&instr); }
        if !done { done = self.loads(&instr); }
        if !done { done = self.stores(&instr); }

        if !done && instr.opcode == Opcode::mv {
            let val = self.regs.get(instr.rs1 as usize);
            self.regs.set(instr.rd as usize, val);
            done = true;
        }

        if !done {
            panic!("unhandled instruction: {:?}", instr);
        }
    }

    pub fn exec_loop(&mut self) -> Option<()> {
        loop {
            let pc = self.get_pc();
            let raw: u64 = self.mem.read_data(pc)?;
            if raw == END_MARKER {
                break;
            }

            let instr = self.get_instruction()?;
            self.inc_pc();
            self.execute(instr);
            //self.regs.print();
        }

        Some(())
    }

    pub fn new(program: Vec<u64>) -> Self {
        let mut res = Self {
            regs: Registers::new(),
            mem: Memory::new(25 * 1024 * 1024), // 25 MiB
        };

        // PC starts as 0x1000, why not.
        res.set_reg(0, 0x1000);
        let mut loc: usize = 0x1000;
        for val in program {
            res.mem.write_data(loc, val);
            loc += 8;
        }
        res.mem.write_data(loc, END_MARKER);

        res
    }
}
