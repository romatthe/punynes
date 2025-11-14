use crate::cpu::Cpu;
use crate::MemRead;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Operand {
    Value(u8),
    Address(u16),
}

/// Each addressing mode is a function taking &mut Cpu and returning Operand (which
/// can be a u8 or u16).
pub type AddrModeFn = fn(&mut Cpu) -> Operand;

/// Absolute mode addressing
///
/// The operand value is the next full 16-bit word.
fn mode_abs(cpu: &mut Cpu) -> Operand {
    let lo = cpu.read(cpu.pc.wrapping_add(1));
    let hi = cpu.read(cpu.pc.wrapping_add(2));
    cpu.pc = cpu.pc.wrapping_add(3);
    Operand::Address(u16::from_le_bytes([lo, hi]))
}

/// Absolute, X-indexed mode addressing
///
/// The operand value is the next full 16-bit word, with the value of the X register added on
/// to it. Page crosses may be handled.
fn mode_abx(cpu: &mut Cpu) -> Operand {
    let lo = cpu.read(cpu.pc.wrapping_add(1));
    let hi = cpu.read(cpu.pc.wrapping_add(2));
    let base = u16::from_le_bytes([lo, hi]);
    let addr = base.wrapping_add(cpu.x as u16);
    cpu.pc = cpu.pc.wrapping_add(3);

    // TODO: Handle page cross here!
    if (base & 0xFF00) != (addr & 0xFF00) {
        // Add additional cycle for page cross here?
    }

    Operand::Address(addr)
}

/// Absolute, Y-indexed mode addressing
///
/// The operand value is the next full 16-bit word, with the value of the Y register added on
/// to it. Page crosses may be handled.
fn mode_aby(cpu: &mut Cpu) -> Operand {
    let lo = cpu.read(cpu.pc.wrapping_add(1));
    let hi = cpu.read(cpu.pc.wrapping_add(2));
    let base = u16::from_le_bytes([lo, hi]);
    let addr = base.wrapping_add(cpu.y as u16);
    cpu.pc = cpu.pc.wrapping_add(3);

    // TODO: Handle page cross here!
    if (base & 0xFF00) != (addr & 0xFF00) {
        // Add additional cycle for page cross here?
    }

    Operand::Address(addr)
}

/// Accumulator mode addressing
///
/// The operand value is simply what is stored in the accumulator.
fn mode_acc(cpu: &mut Cpu) -> Operand {
    cpu.pc = cpu.pc.wrapping_add(1);
    Operand::Value(cpu.a)
}

/// X-indexed, indirect addressing
///
/// The operand address is acquired by reading the next byte, adding the X register to it (wrapping
/// at 0xFF). This results in a pointer into the Zero-Page, from which we read two LE bytes, which
/// gives the final address.
fn mode_idx(cpu: &mut Cpu) -> Operand {
    let zp = cpu.read(cpu.pc.wrapping_add(1));
    let zp = zp.wrapping_add(cpu.x); // wraps around at 0xFF
    let lo = cpu.read(u16::from_le_bytes([zp, 0x00]));
    let hi = cpu.read(u16::from_le_bytes([zp.wrapping_add(1), 0x00])); // high bytes wraps around 0xFF

    cpu.pc = cpu.pc.wrapping_add(2);
    Operand::Address(u16::from_le_bytes([lo, hi]))
}

/// Indirect addressing, Y-indexed
///
/// The operand address is acquired by reading the next byte as a pointer into the Zero-Page,
/// from which we read two LE bytes, which gives the final address. We add the value of the Y register
/// to this in order to get the correct address.
fn mode_idy(cpu: &mut Cpu) -> Operand {
    let zp = cpu.read(cpu.pc.wrapping_add(1));
    let lo = cpu.read(u16::from_le_bytes([zp, 0x00]));
    let hi = cpu.read(u16::from_le_bytes([zp.wrapping_add(1), 0x00])); // high bytes wraps around 0xFF

    let addr = u16::from_le_bytes([lo, hi]);
    let addr = addr.wrapping_add(cpu.y as u16);

    cpu.pc = cpu.pc.wrapping_add(2);
    Operand::Address(addr)
}

/// Immediate mode addressing
///
/// The next byte is used as the operand value.
fn mode_imm(cpu: &mut Cpu) -> Operand {
    let val = cpu.read(cpu.pc.wrapping_add(1));
    cpu.pc = cpu.pc.wrapping_add(2);
    Operand::Value(val)
}

/// Implied mode addressing
///
/// The operand value is implicitly defined by the instruction. We return nothing useful.
fn mode_imp(cpu: &mut Cpu) -> Operand {
    cpu.pc = cpu.pc.wrapping_add(1);
    Operand::Value(0) // TODO: Is there a better alternative? Adding an enum adds a match arm.
}

/// Relative mode addressing
///
/// The operand address is the next read byte (u8), interpreted as a signed byte (i8), added to
/// the value of the PC.
fn mode_rel(cpu: &mut Cpu) -> Operand {
    let offset = cpu.read(cpu.pc.wrapping_add(1)) as i8;
    cpu.pc = cpu.pc.wrapping_add(2);
    // Note: the i8 is first cast to an i16, so the signedness is retained
    let i = cpu.pc.wrapping_add(offset as u16);
    Operand::Address(i)
}

/// Zero-Page mode addressing, X-indexed addressing
///
/// The operand is an address formed with the next read byte as the low byte, and 0x00 as the
/// high byte. This allows us to access the "Zero-Page" (the first 0xFF bytes in memory) by reading
/// only a single byte.
fn mode_zp0(cpu: &mut Cpu) -> Operand {
    let lo = cpu.read(cpu.pc.wrapping_add(1));
    cpu.pc = cpu.pc.wrapping_add(2);
    Operand::Address(u16::from_le_bytes([lo, 0x00]))
}

/// Zero-Page mode addressing, X-indexed addressing
///
/// Same as Zero-Page addressing, but we add the value of the X register to the low byte first,
/// wrapping around at 0xFF so we always stay within the Zero-Page.
fn mode_zpx(cpu: &mut Cpu) -> Operand {
    let lo = cpu.read(cpu.pc.wrapping_add(1));
    let lo = lo.wrapping_add(cpu.x); // wraps around at 0xFF
    cpu.pc = cpu.pc.wrapping_add(2);
    Operand::Address(u16::from_le_bytes([lo, 0x00]))
}

/// Zero-Page mode addressing, Y-indexed addressing
///
/// Same as Zero-Page addressing, but we add the value of the Y register to the low byte first,
/// wrapping around at 0xFF so we always stay within the Zero-Page.
fn mode_zpy(cpu: &mut Cpu) -> Operand {
    let lo = cpu.read(cpu.pc.wrapping_add(1));
    let lo = lo.wrapping_add(cpu.y); // wraps around at 0xFF
    cpu.pc = cpu.pc.wrapping_add(2);
    Operand::Address(u16::from_le_bytes([lo, 0x00]))
}

#[cfg(test)]
mod tests {
    use crate::MemWrite;
    use super::*;

    #[test]
    fn test_mode_abs() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.bus.write(0x0401, 0x42);
        cpu.bus.write(0x0402, 0x16);

        // When
        let operand = mode_abs(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0x1642));  // Correct absolute value
        assert_eq!(cpu.pc, 0x403);                      // PC incremented by 3
    }

    #[test]
    fn test_mode_abx() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.x = 0x08;
        cpu.bus.write(0x0401, 0x42);
        cpu.bus.write(0x0402, 0x16);

        // When
        let operand = mode_abx(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0x164A));  // Correct absolute, X-indexed value
        assert_eq!(cpu.pc, 0x403);                      // PC incremented by 3
    }

    #[test]
    fn test_mode_aby() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.y = 0x08;
        cpu.bus.write(0x0401, 0x42);
        cpu.bus.write(0x0402, 0x16);

        // When
        let operand = mode_aby(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0x164A));  // Correct absolute, X-indexed value
        assert_eq!(cpu.pc, 0x403);                      // PC incremented by 3
    }

    #[test]
    fn test_mode_acc() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.a = 0x66;

        // When
        let operand = mode_acc(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Value(0x66));  // Correct absolute value
        assert_eq!(cpu.pc, 0x401);                  // PC incremented by 3
    }

    #[test]
    fn test_mode_idx() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x0000;
        cpu.bus.write(0x0001, 0x20);

        cpu.x = 0x06; // 0x20 + 0x06 = 0x26

        // Zero-Page pointer:
        cpu.write(0x0026, 0xCD); // low byte
        cpu.write(0x0027, 0xAB); // high byte

        // When
        let operand = mode_idx(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0xABCD));  // Correct X-indexed indirect value
        assert_eq!(cpu.pc, 0x0002);                     // PC incremented by 2
    }

    #[test]
    fn test_mode_idx_wrap_high_byte() {
        let mut cpu = Cpu::new();
        cpu.pc = 0x0002;
        cpu.bus.write(0x0003, 0xFC); // operand

        cpu.x = 0x03; // FC + 03 = FF

        // Zero-page wrap on high byte
        cpu.write(0x00FF, 0x34); // low byte
        cpu.write(0x0000, 0x12); // high byte wraps to $0000, not $0100

        let operand = mode_idx(&mut cpu);

        assert_eq!(operand, Operand::Address(0x1234));  // Pointer should wrap: hi from $0000
        assert_eq!(cpu.pc, 0x0004);                     // PC incremented by 2
    }

    #[test]
    fn test_mode_idx_zero_page_operand_wrap() {
        let mut cpu = Cpu::new();
        cpu.pc = 0x0004;
        cpu.bus.write(0x0005, 0xF0); // base operand

        cpu.x = 0x20; // F0 + 20 = 10 (wrap)

        cpu.bus.write(0x0010, 0xAA); // low byte
        cpu.bus.write(0x0011, 0xBB); // high byte

        let operand = mode_idx(&mut cpu);

        assert_eq!(operand, Operand::Address(0xBBAA));
        assert_eq!(cpu.pc, 0x0006);                     // PC incremented by 2
    }

    #[test]
    fn test_mode_idy() {
        let mut cpu = Cpu::new();

        // Instruction at $0000: LDA ($10),Y
        cpu.pc = 0x0000;
        cpu.bus.write(0x0001, 0x10);

        cpu.y = 0x05;

        // Zero-page pointer
        cpu.write(0x0010, 0x34); // low byte
        cpu.write(0x0011, 0x12); // high byte

        let operand = mode_idy(&mut cpu);

        // Effective address = 0x1234 + 0x05 = 0x1239
        assert_eq!(operand, Operand::Address(0x1239));
        assert_eq!(cpu.pc, 0x0002);                     // PC incremented by 2
    }

    #[test]
    fn test_mode_idy_zero_page_wrap() {
        let mut cpu = Cpu::new();

        // Instruction at $0002: LDA ($FF),Y
        cpu.pc = 0x0002;
        cpu.bus.write(0x0003, 0xFF);

        cpu.y = 0x01;

        // Zero-page pointer wraps
        cpu.write(0x00FF, 0xAA); // low byte
        cpu.write(0x0000, 0xBB); // high byte

        let operand = mode_idy(&mut cpu);

        // Effective address = 0xBBAA + 0x01 = 0xBBAB
        assert_eq!(operand, Operand::Address(0xBBAB));
        assert_eq!(cpu.pc, 0x0004);                     // PC incremented by 2
    }

    #[test]
    fn test_mode_imm() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.bus.write(0x0401, 0x42);

        // When
        let operand = mode_imm(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Value(0x42));  // Correct immediate value
        assert_eq!(cpu.pc, 0x402);                  // PC incremented by 2
    }

    #[test]
    fn test_mode_imp() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.bus.write(0x0401, 0x42);

        // When
        let operand = mode_imp(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Value(0x0));  // Correct implied value // TODO: Makes sense?
        assert_eq!(cpu.pc, 0x401);                 // PC incremented by 1
    }

    #[test]
    fn test_mode_rel() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.bus.write(0x0401, 0x80);

        // When
        let operand = mode_rel(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0x382));   // Correct relative value
        assert_eq!(cpu.pc, 0x402);                      // PC incremented by 2
    }

    #[test]
    fn test_mode_zp0() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.bus.write(0x0401, 0x42);

        // When
        let operand = mode_zp0(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0x0042));  // Correct zero-page value
        assert_eq!(cpu.pc, 0x402);                      // PC incremented by 2
    }

    #[test]
    fn test_mode_zpx() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.x = 0x20;
        cpu.bus.write(0x0401, 0xF0);

        // When
        let operand = mode_zpx(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0x0010));  // Correct zero-page, x-indexed value
        assert_eq!(cpu.pc, 0x402);                      // PC incremented by 2
    }

    #[test]
    fn test_mode_zpy() {
        // Given
        let mut cpu = Cpu::new();
        cpu.pc = 0x400;
        cpu.y = 0x20;
        cpu.bus.write(0x0401, 0xF0);

        // When
        let operand = mode_zpy(&mut cpu);

        // Then
        assert_eq!(operand, Operand::Address(0x0010));  // Correct zero-page, y-indexed value
        assert_eq!(cpu.pc, 0x402);                      // PC incremented by 2
    }

    // proptest! {
    //     #[test]
    //     fn mode_imm_increments_pc_correctly(pc in 0u16..0xFFFE, val in any::<u8>()) {
    //         let mut cpu = setup_cpu(pc);
    //         cpu.bus.write(pc.wrapping_add(1), val);
    //
    //         let operand = mode_imm(&mut cpu);
    //
    //         prop_assert_eq!(operand, Operand::Value(val));
    //         prop_assert_eq!(cpu.pc, pc.wrapping_add(2));
    //     }
    // }
}