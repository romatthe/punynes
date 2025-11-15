use crate::{MemRead, MemWrite};

#[derive(Debug)]
pub struct Bus {
    ram: [u8; 0x0800], // NES has 2KB internal RAM
}

impl Bus {
    pub fn new() -> Self {
        Self {
            ram: [0x0; 0x0800],
        }
    }
}

impl MemRead for Bus {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            // Working RAM
            0x0000..=0x07FF => self.ram[addr as usize],
            _ => todo!("Implement correct memory mapping in bus!")
        }
    }
}

impl MemWrite for Bus {
    fn write(&mut self, addr: u16, byte: u8) {
        match addr {
            0x0000..=0x07FF => self.ram[addr as usize] = byte,
            _ => todo!("Implement correct memory mapping in bus!")
        }
    }
}