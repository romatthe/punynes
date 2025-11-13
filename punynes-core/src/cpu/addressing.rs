use crate::cpu::Cpu;
use crate::MemRead;

#[derive(Debug, Clone, Copy)]
pub enum Operand {
    Value(u8),
    Address(u16),
}

/// Each addressing mode is a function taking &mut Cpu and returning Operand (which
/// can be a u8 or u16).
pub type AddrModeFn = fn(&mut Cpu) -> Operand;

/// Implied mode addressing
///
/// The operand value is implicitly defined by the instruction. We return nothing useful.
fn mode_imp(cpu: &mut Cpu) -> Operand {
    cpu.pc = cpu.pc.wrapping_add(1);
    Operand::Value(0) // TODO: Is there a better alternative? Adding an enum adds a match arm.
}

/// Absolute mode addressing
///
/// The operand value is the next full 16-bit word.
fn mode_abs(cpu: &mut Cpu) -> Operand {
    let lo = cpu.read(cpu.pc + 1);
    let hi = cpu.read(cpu.pc + 2);
    cpu.pc = cpu.pc.wrapping_add(3);
    Operand::Address(u16::from_le_bytes([lo, hi]))
}

/// Accumulator mode addressing
///
/// The operaend value is simply what is stored in the accumulator.
fn mode_acc(cpu: &mut Cpu) -> Operand {
    cpu.pc = cpu.pc.wrapping_add(1);
    Operand::Value(cpu.a)
}

/// Immediate mode addressing
///
/// The next byte is used as the operand value.
fn mode_imm(cpu: &mut Cpu) -> Operand {
    let val = cpu.read(cpu.pc + 1);
    cpu.pc = cpu.pc.wrapping_add(2);
    Operand::Value(val)
}