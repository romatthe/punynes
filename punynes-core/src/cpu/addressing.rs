use crate::cpu::Cpu;

#[derive(Debug, Clone, Copy)]
enum Operand {
    Value(u8),
    Address(u16),
}

// Each addressing mode is a function taking &mut Cpu and returning Operand.
pub type AddrModeFn = fn(&mut Cpu) -> Operand;

/// Immediate mode addressing
fn mode_imm(cpu: &mut Cpu) -> Operand {
    let val = cpu.read_byte(cpu.pc + 1);
    cpu.pc += 2;
    Operand::Value(val)
}