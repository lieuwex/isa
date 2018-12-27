use std::{fmt, result};

pub struct Registers {
    regs: [u64; 16],
}

impl Registers {
    pub fn get(&self, i: usize) -> u64 {
        self.regs[i]
    }

    pub fn set(&mut self, i: usize, val: u64) {
        if i >= self.regs.len() {
            return;
        }

        self.regs[i] = val;
    }

    pub fn new() -> Self {
        Self { regs: [0; 16] }
    }
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        for (i, reg) in self.regs.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }

            write!(f, "r{}={}", i, reg)?;
        }

        Ok(())
    }
}
