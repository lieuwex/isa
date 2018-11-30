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

    pub fn print(&self) {
        for (i, reg) in self.regs.iter().enumerate() {
            println!("{} {}", i, reg);
        }
    }

    pub fn new() -> Self {
        Self {
            regs: [0; 16],
        }
    }
}
