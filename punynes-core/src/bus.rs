use crate::MemRead;

#[derive(Debug)]
pub struct Bus {
    ram: [u8; 0x0800], // NES has 2KB internal RAM
}

impl MemRead for Bus {
    fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x07FF => self.ram[addr as usize],
            _ => todo!("Implement correct memory mapping in bus!")
        }
    }
}