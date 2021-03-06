use std::{
    io::{self, Read, Write},
    mem,
};
use crate::{debug::DebugMode};

pub struct Memory {
    data: Vec<u8>,
    debug_mode: DebugMode,
}

impl Memory {
    pub fn read_data<T: Copy>(&self, addr: usize) -> Option<T> {
        if addr == 81 {
            let mut buf = [0; 1];
            let n = io::stdin().read(&mut buf).unwrap();
            let c = if n == 0 {
                u64::max_value()
            } else {
                u64::from(buf[0])
            };
            let val: T = unsafe { mem::transmute_copy::<u64, T>(&c) };
            return Some(val);
        } else if addr >= self.data.len() ||
                    addr + mem::size_of::<T>() > self.data.len() {
            return None;
        }

        if self.debug_mode == DebugMode::Lackey {
            println!("L {:X},{}", addr, mem::size_of::<T>());
        }

        let data = self.data.as_ptr();
        unsafe {
            let ptr = data.add(addr) as *const T;
            Some(ptr.read())
        }
    }

    pub fn write_data<T>(&mut self, addr: usize, val: T) -> bool {
        if addr == 80 { // or whatever
            let c: u8 = unsafe { mem::transmute_copy::<T, u8>(&val) };
            let cbuf = [c; 1];
            return io::stdout().write_all(&cbuf).is_ok();
        } else if addr >= self.data.len() ||
                    addr + mem::size_of::<T>() > self.data.len() {
            return false;
        }

        if self.debug_mode == DebugMode::Lackey {
            println!("S {:X},{}", addr, mem::size_of::<T>());
        }

        let data = self.data.as_mut_ptr();
        unsafe {
            let ptr = data.add(addr) as *mut T;
            ptr.write(val);
        }

        true
    }

    pub fn new(size: usize, debug_mode: DebugMode) -> Self {
        Self {
            data: vec![0; size],
            debug_mode,
        }
    }
}
