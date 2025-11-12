use crate::cpu::addressing::AddrModeFn;

mod addressing;

enum Register8 { A, X, Y }

#[derive(Debug)]
struct Cpu {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
}

impl Cpu {
    fn ld(cpu: &mut Cpu, mode: AddrModeFn, reg: Register8) {
        // Implement indexing on CPU? So you can index by Register8?
        // match mode(cpu) {
        //     Operand::Value(val) => cpu[reg] = val,
        //     Operand::Address(addr) => cpu[reg] = cpu.read_byte(addr),
        // }
        match mode(cpu) {
            Operand::Value(v) => cpu.a = v,
            Operand::Address(addr) => cpu.a = cpu.read_byte(addr),
        }
        cpu.update_zn_flags(cpu.a);
    }
}