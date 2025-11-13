mod cpu;
mod bus;

pub trait MemRead {
    fn read(&self, addr: u16) -> u8;
}

pub trait MemWrite {
    fn write(&mut self, addr: u16, byte: u8);
}
