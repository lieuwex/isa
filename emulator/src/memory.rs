pub struct Memory {
    data: Vec<u8>,
}

impl Memory {
    pub fn read_data<T: Copy>(&self, addr: usize) -> T {
        let data = self.data.as_ptr();
        unsafe {
            let ptr = data.add(addr) as *const T;
            ptr.read()
        }
    }

    pub fn write_data<T>(&mut self, addr: usize, val: T) {
        let data = self.data.as_mut_ptr();
        unsafe {
            let ptr = data.add(addr) as *mut T;
            ptr.write(val);
        }
    }

    pub fn new(size: usize) -> Self {
        Self {
            data: vec![0; size],
        }
    }
}
