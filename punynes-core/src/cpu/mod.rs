use std::ops::{Index, IndexMut};
use crate::bus::Bus;
use crate::cpu::addressing::{AddrModeFn, Operand};
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
    pub fn new() -> Self {
        Self {
            bus: Bus::new(),
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            status: 0,
        }
    }

    fn ld(cpu: &mut Cpu, mode: AddrModeFn, reg: Register8) {
        match mode(cpu) {
            Operand::Value(val) => cpu[reg] = val,
            Operand::Address(addr) => cpu[reg] = cpu.read(addr),
        }
        // TODO:
        // cpu.update_zn_flags(cpu.a);
    }
}

impl Index<Register8> for Cpu {
    type Output = u8;

    fn index(&self, reg: Register8) -> &Self::Output {
        match reg {
            Register8::A => &self.a,
            Register8::X => &self.x,
            Register8::Y => &self.y,
        }
    }
}

impl IndexMut<Register8> for Cpu {
    fn index_mut(&mut self, reg: Register8) -> &mut Self::Output {
        match reg {
            Register8::A => &mut self.a,
            Register8::X => &mut self.x,
            Register8::Y => &mut self.y,
        }
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