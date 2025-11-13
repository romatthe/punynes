use crate::cpu::Cpu;
use crate::MemRead;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Value(u8),
    Address(u16),
}

// Each addressing mode is a function taking &mut Cpu and returning Operand.
pub type AddrModeFn = fn(&mut Cpu) -> Operand;

/// Immediate mode addressing
fn mode_imm(cpu: &mut Cpu) -> Operand {
    let val = cpu.read(cpu.pc + 1);
    cpu.pc += 2;
    Operand::Value(val)
}