use registers::Registers;
use instruction::*;
use memory::Memory;
use opcode::Opcode;
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
    pub fn get_instruction(&self) -> Instruction {
        let pc = self.get_pc();
        let raw: u64 = self.mem.read_data(pc);
        instruction_decode(raw)
    }

    pub fn set_reg(&mut self, i: usize, val: u64) {
        self.regs.set(i, val);
    }
    pub fn inc_pc(&mut self) {
        let pc = self.get_pc() as u64;
        self.set_reg(0, pc + 8);
    }

    pub fn arithmatic(&mut self, instr: &Instruction) -> bool {
        let a: u64 = self.get_reg(instr.rs1 as usize);
        let b: u64 = self.get_reg(instr.rs2 as usize);

        let res: u64;
        match instr.opcode {
            Opcode::not => res = !a,
            Opcode::add => res = a + b,
            Opcode::sub => res = a - b,
            Opcode::mul => res = a * b,
            Opcode::div => res = a / b,
            Opcode::and => res = a & b,
            Opcode::or => res = a | b,
            Opcode::xor => res = a ^ b,
            Opcode::sll => res = a << b,
            Opcode::slr => res = a >> b,
            Opcode::sar => res = a >> b, // TODO

            _ => return false,
        };

        self.set_reg(instr.rd as usize, res);
        true
    }
    pub fn jumps(&mut self, instr: &Instruction) -> bool {
        let usereg = match instr.opcode {
            Opcode::jnzr | Opcode::jzr => true,
            _ => false,
        };

        let cond = self.regs.get(instr.rs1 as usize);
        let dest: u64 = if usereg {
            self.regs.get(instr.rs2 as usize)
        } else {
            instr.immediate as u64
        };

        let matches;
        match instr.opcode {
            Opcode::call => {
                matches = true;
                let pc = self.get_pc() as u64;
                self.regs.set(instr.rd as usize, pc);
            }
            Opcode::jnz | Opcode::jnzr => matches = cond != 0,
            Opcode::jz | Opcode::jzr => matches = cond == 0,

            _ => return false,
        }

        if matches {
            self.regs.set(0, dest);
        }
        true
    }
    pub fn loads(&mut self, instr: &Instruction) -> bool {
        let rs1: usize = self.regs.get(instr.rs1 as usize) as usize;

        let res: u64;
        match instr.opcode {
            Opcode::li => res = instr.immediate as u64,

            Opcode::l8 => {
                let raw: u8 = self.mem.read_data(rs1);
                res = sign_extend(raw as u64, 8)
            }
            Opcode::l16 => {
                let raw: u16 = self.mem.read_data(rs1);
                res = sign_extend(raw as u64, 16)
            },
            Opcode::l32 => {
                let raw: u32 = self.mem.read_data(rs1);
                res = sign_extend(raw as u64, 32)
            },
            Opcode::l64 => {
                let raw: u64 = self.mem.read_data(rs1);
                res = sign_extend(raw, 64)
            },

            _ => return false,
        }

        self.regs.set(instr.rd as usize, res);
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
        }

        true
    }

    pub fn execute(&mut self, instr: Instruction) {
        let mut done = false;
        if !done { done = self.arithmatic(&instr); }
        if !done { done = self.jumps(&instr); }
        if !done { done = self.loads(&instr); }
        if !done { done = self.stores(&instr); }

        if !done && instr.opcode == Opcode::mv {
            let val = self.regs.get(instr.rs2 as usize);
            self.regs.set(instr.rs1 as usize, val);
            done = true;
        }

        if !done {
            panic!("unhandled instruction: {:?}", instr);
        }
    }

    pub fn exec_loop(&mut self) {
        loop {
            let pc = self.get_pc();
            println!("{}", pc);
            let raw: u64 = self.mem.read_data(pc);
            if raw == END_MARKER {
                break;
            }

            let instr = self.get_instruction();
            self.inc_pc();
            self.execute(instr);
            self.regs.print();
        }
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
