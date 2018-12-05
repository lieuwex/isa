use std::mem;

pub struct Memory {
    data: Vec<u8>,
}

impl Memory {
    pub fn read_data<T: Copy>(&self, addr: usize) -> Option<T> {
        if addr >= self.data.len() {
            return None;
        }

        let data = self.data.as_ptr();
        unsafe {
            let ptr = data.add(addr) as *const T;
            Some(ptr.read())
        }
    }

    pub fn write_data<T>(&mut self, addr: usize, val: T) -> bool {
        if addr == 80 { // or whatever
            let c: u8 = unsafe {
                mem::transmute_copy::<T, u8>(&val)
            };
            print!("{}", c as char);
            return true;
        } else if addr >= self.data.len() {
            return false;
        }

        let data = self.data.as_mut_ptr();
        unsafe {
            let ptr = data.add(addr) as *mut T;
            ptr.write(val);
        }

        true
    }

    pub fn new(size: usize) -> Self {
        Self {
            data: vec![0; size],
        }
    }
}
