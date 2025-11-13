use crate::bus::Bus;
use crate::cpu::addressing::AddrModeFn;
use crate::{MemRead, MemWrite};

mod addressing;

enum Register8 { A, X, Y }

#[derive(Debug)]
struct Cpu {
    bus: Bus,
    pc: u16,
    sp: u8,
    a: u8,
    x: u8,
    y: u8,
    status: u8,
}

impl Cpu {
    fn ld(cpu: &mut Cpu, mode: AddrModeFn, reg: Register8) {
        // Implement indexing on CPU? So you can index by Register8?
        // match mode(cpu) {
        //     Operand::Value(val) => cpu[reg] = val,
        //     Operand::Address(addr) => cpu[reg] = cpu.read_byte(addr),
        // }
        // cpu.update_zn_flags(cpu.a);
    }
}

impl MemRead for Cpu {
    fn read(&self, addr: u16) -> u8 {
        self.bus.read(addr)
    }
}

impl MemWrite for Cpu {
    fn write(&mut self, addr: u16, byte: u8) {
        self.bus.write(addr, byte);
    }
}