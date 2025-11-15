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
    use proptest::prelude::*;
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
    fn test_mode_idx_manual() {
        let mut cpu = Cpu::new();

        // Set PC and X register
        cpu.pc = 0x10;   // arbitrary WRAM address
        cpu.x = 0x03;    // arbitrary X offset

        // Write the operand for the instruction at PC + 1
        cpu.bus.write(cpu.pc + 1, 0xFC); // base zero-page pointer

        // Compute the zero-page address after adding X (wrapping at 0xFF)
        let zp_addr = 0xFCu8.wrapping_add(cpu.x); // 0xFC + 0x03 = 0xFF

        // Write low and high bytes of the target address
        cpu.bus.write(zp_addr as u16, 0x34);                 // low byte
        cpu.bus.write((zp_addr.wrapping_add(1) & 0xFF) as u16, 0x12); // high byte wraps to $00

        // Call the addressing mode
        let operand = mode_idx(&mut cpu);

        // Print debugging info
        println!("PC before: 0x10, PC after: {:04X}", cpu.pc);
        println!("Zero-page pointer: {:02X}", zp_addr);
        println!("Expected address: 0x1234");
        println!("Returned operand: {:?}", operand);

        assert_eq!(operand, Operand::Address(0x1234));
        assert_eq!(cpu.pc, 0x12); // PC incremented by 2
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
}

#[cfg(test)]
mod proptests {
    use super::*;
    use crate::MemWrite;
    use proptest::prelude::*;

    /// Helper to create a CPU with specific initial state
    fn cpu_with_state(pc: u16, a: u8, x: u8, y: u8, memory: &[(u16, u8)]) -> Cpu {
        let mut cpu = Cpu::new();
        cpu.pc = pc;
        cpu.a = a;
        cpu.x = x;
        cpu.y = y;

        // Write test data to memory (already filtered to valid WRAM range)
        for &(addr, val) in memory {
            if addr <= 0x07FF {
                cpu.write(addr, val);
            }
        }

        cpu
    }

    /// Strategy for valid WRAM addresses (0x0000 - 0x07FF)
    fn wram_addr() -> impl Strategy<Value = u16> {
        0x0000u16..=0x07FF
    }

    /// Strategy for PC that allows reading 1 byte after it (PC+1 must be in WRAM)
    fn safe_pc_1byte() -> impl Strategy<Value = u16> {
        0x0000u16..=0x07FE
    }

    /// Strategy for PC that allows reading 2 bytes after it (PC+2 must be in WRAM)
    fn safe_pc_2bytes() -> impl Strategy<Value = u16> {
        0x0000u16..=0x07FD
    }

    // ABSOLUTE MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_abs_pc_advances_by_3(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            addr_lo in any::<u8>(),
            addr_hi in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), addr_lo),
                (initial_pc.wrapping_add(2), addr_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_abs(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(3));
        }

        #[test]
        fn prop_test_abs_returns_correct_address(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            addr_lo in any::<u8>(),
            addr_hi in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), addr_lo),
                (initial_pc.wrapping_add(2), addr_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_abs(&mut cpu);
            let expected_addr = u16::from_le_bytes([addr_lo, addr_hi]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }
    }

    // ABSOLUTE,X MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_abx_pc_advances_by_3(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            addr_lo in any::<u8>(),
            addr_hi in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), addr_lo),
                (initial_pc.wrapping_add(2), addr_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_abx(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(3));
        }

        #[test]
        fn prop_test_abx_returns_address_plus_x(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            addr_lo in any::<u8>(),
            addr_hi in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), addr_lo),
                (initial_pc.wrapping_add(2), addr_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_abx(&mut cpu);
            let base_addr = u16::from_le_bytes([addr_lo, addr_hi]);
            let expected_addr = base_addr.wrapping_add(x as u16);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }

        #[test]
        fn prop_test_abx_wraps_correctly(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in 0x01u8..=0xFF,
            y in any::<u8>(),
        ) {
            // Set up address that will wrap (0xFFFF + X wraps to 0x00XX)
            let memory = vec![
                (initial_pc.wrapping_add(1), 0xFF),
                (initial_pc.wrapping_add(2), 0xFF),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_abx(&mut cpu);
            let expected_addr = 0xFFFFu16.wrapping_add(x as u16);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }
    }

    // ABSOLUTE,Y MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_aby_pc_advances_by_3(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            addr_lo in any::<u8>(),
            addr_hi in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), addr_lo),
                (initial_pc.wrapping_add(2), addr_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_aby(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(3));
        }

        #[test]
        fn prop_test_aby_returns_address_plus_y(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            addr_lo in any::<u8>(),
            addr_hi in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), addr_lo),
                (initial_pc.wrapping_add(2), addr_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_aby(&mut cpu);
            let base_addr = u16::from_le_bytes([addr_lo, addr_hi]);
            let expected_addr = base_addr.wrapping_add(y as u16);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }
    }

    // ACCUMULATOR MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_acc_pc_advances_by_1(
            initial_pc in wram_addr(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
        ) {
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &[]);

            mode_acc(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(1));
        }

        #[test]
        fn prop_test_acc_returns_accumulator_value(
            initial_pc in wram_addr(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
        ) {
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &[]);

            let result = mode_acc(&mut cpu);

            prop_assert_eq!(result, Operand::Value(a));
        }
    }

    // X-INDEXED, INDIRECT MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_idx_pc_advances_by_2(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_base in any::<u8>(),
            target_lo in any::<u8>(),
            target_hi in any::<u8>(),
        ) {
            let zp_addr = zp_base.wrapping_add(x);

            // Prevent memory address collisions
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr as u16);
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr.wrapping_add(1) as u16);

            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
                (zp_addr as u16, target_lo),
                (zp_addr.wrapping_add(1) as u16, target_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_idx(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(2));
        }

        #[test]
        fn prop_test_idx_returns_correct_address(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_base in any::<u8>(),
            target_lo in any::<u8>(),
            target_hi in any::<u8>(),
        ) {
            let zp_addr = zp_base.wrapping_add(x);

            // Prevent memory address collisions
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr as u16);
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr.wrapping_add(1) as u16);

            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
                (zp_addr as u16, target_lo),
                (zp_addr.wrapping_add(1) as u16, target_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_idx(&mut cpu);
            let expected_addr = u16::from_le_bytes([target_lo, target_hi]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }

        #[test]
        fn test_idx_wraps_in_zero_page(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in 0x01u8..=0xFF,
            y in any::<u8>(),
            target_lo in any::<u8>(),
            target_hi in any::<u8>(),
        ) {
            // Use 0xFF as base, which with any X > 0 will wrap
            let zp_base: u8 = 0xFF;
            let zp_addr = zp_base.wrapping_add(x); // Will wrap to 0x00..0xFE

            // Ensure initial_pc + 1 doesn't collide with zp_addr or zp_addr + 1
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr as u16);
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr.wrapping_add(1) as u16);

            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
                (zp_addr as u16, target_lo),
                (zp_addr.wrapping_add(1) as u16, target_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_idx(&mut cpu);
            let expected_addr = u16::from_le_bytes([target_lo, target_hi]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }
    }

    // INDIRECT, Y-INDEXED MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_idy_pc_advances_by_2(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_addr in any::<u8>(),
            base_lo in any::<u8>(),
            base_hi in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_addr),
                (zp_addr as u16, base_lo),
                (zp_addr.wrapping_add(1) as u16, base_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_idy(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(2));
        }

        #[test]
        fn test_idy_returns_address_plus_y(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_addr in any::<u8>(),
            base_lo in any::<u8>(),
            base_hi in any::<u8>(),
        ) {
            // Prevent memory address collisions
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr as u16);
            prop_assume!(initial_pc.wrapping_add(1) != zp_addr.wrapping_add(1) as u16);

            let memory = vec![
                (initial_pc.wrapping_add(1), zp_addr),
                (zp_addr as u16, base_lo),
                (zp_addr.wrapping_add(1) as u16, base_hi),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_idy(&mut cpu);
            let base_addr = u16::from_le_bytes([base_lo, base_hi]);
            let expected_addr = base_addr.wrapping_add(y as u16);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }

        #[test]
        fn prop_test_idy_zp_pointer_wraps(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            base_lo in any::<u8>(),
            base_hi in any::<u8>(),
        ) {
            // Use 0xFF as ZP address to test wrapping
            let zp_addr = 0xFF;

            // Prevent collision: initial_pc + 1 must not be 0xFF
            prop_assume!(initial_pc.wrapping_add(1) != 0xFF);

            let memory = vec![
                (initial_pc.wrapping_add(1), zp_addr),
                (0xFF, base_lo), // At 0xFF
                (0x00, base_hi), // Wraps to 0x00
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_idy(&mut cpu);
            let base_addr = u16::from_le_bytes([base_lo, base_hi]);
            let expected_addr = base_addr.wrapping_add(y as u16);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }
    }

    // IMMEDIATE MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_imm_pc_advances_by_2(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            immediate_val in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), immediate_val),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_imm(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(2));
        }

        #[test]
        fn prop_test_imm_returns_immediate_value(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            immediate_val in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), immediate_val),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_imm(&mut cpu);

            prop_assert_eq!(result, Operand::Value(immediate_val));
        }
    }

    // IMPLIED MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_imp_pc_advances_by_1(
            initial_pc in wram_addr(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
        ) {
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &[]);

            mode_imp(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(1));
        }

        #[test]
        fn prop_test_imp_returns_value_zero(
            initial_pc in wram_addr(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
        ) {
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &[]);

            let result = mode_imp(&mut cpu);

            prop_assert_eq!(result, Operand::Value(0));
        }
    }

    // RELATIVE MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_rel_pc_advances_by_2(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            offset in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), offset),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_rel(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(2));
        }

        #[test]
        fn prop_test_rel_positive_offset(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            offset in 0x00u8..=0x7F, // Positive offsets
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), offset),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_rel(&mut cpu);
            // PC is incremented by 2 before adding offset
            let expected_addr = initial_pc.wrapping_add(2).wrapping_add(offset as u16);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }

        #[test]
        fn prop_test_rel_negative_offset(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            offset in 0x80u8..=0xFF, // Negative offsets when cast to i8
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), offset),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_rel(&mut cpu);
            let signed_offset = offset as i8;
            // PC is incremented by 2 before adding offset
            let expected_addr = initial_pc.wrapping_add(2).wrapping_add(signed_offset as u16);

            prop_assert_eq!(result, Operand::Address(expected_addr));
        }
    }

    // ZERO-PAGE MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_zp0_pc_advances_by_2(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_addr in any::<u8>(),
        ) {
            println!("Testing value: {}", x); // log the input
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_addr),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_zp0(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(2));
        }

        #[test]
        fn prop_test_zp0_returns_zero_page_address(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_addr in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_addr),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_zp0(&mut cpu);
            let expected_addr = u16::from_le_bytes([zp_addr, 0x00]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
            // Verify it's in zero page
            prop_assert!(expected_addr < 0x0100);
        }
    }

    // ZERO-PAGE,X MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_zpx_pc_advances_by_2(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_base in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_zpx(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(2));
        }

        #[test]
        fn prop_test_zpx_returns_zero_page_address_plus_x(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_base in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_zpx(&mut cpu);
            let zp_addr = zp_base.wrapping_add(x);
            let expected_addr = u16::from_le_bytes([zp_addr, 0x00]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
            // Verify it's in zero page (wraps)
            prop_assert!(expected_addr < 0x0100);
        }

        #[test]
        fn prop_test_zpx_wraps_at_page_boundary(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in 0x01u8..=0xFF,
            y in any::<u8>(),
        ) {
            // Use 0xFF as base to guarantee wrapping
            let zp_base = 0xFF;
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_zpx(&mut cpu);
            let zp_addr = zp_base.wrapping_add(x);
            let expected_addr = u16::from_le_bytes([zp_addr, 0x00]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
            // Should still be in zero page after wrapping
            prop_assert!(expected_addr < 0x0100);
        }
    }

    // ZERO-PAGE,Y MODE PROPTESTS
    proptest! {
        #[test]
        fn prop_test_zpy_pc_advances_by_2(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_base in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            mode_zpy(&mut cpu);

            prop_assert_eq!(cpu.pc, initial_pc.wrapping_add(2));
        }

        #[test]
        fn prop_test_zpy_returns_zero_page_address_plus_y(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
            zp_base in any::<u8>(),
        ) {
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_zpy(&mut cpu);
            let zp_addr = zp_base.wrapping_add(y);
            let expected_addr = u16::from_le_bytes([zp_addr, 0x00]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
            // Verify it's in zero page (wraps)
            prop_assert!(expected_addr < 0x0100);
        }

        #[test]
        fn prop_test_zpy_wraps_at_page_boundary(
            initial_pc in safe_pc_1byte(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in 0x01u8..=0xFF,
        ) {
            // Use 0xFF as base to guarantee wrapping
            let zp_base = 0xFF;
            let memory = vec![
                (initial_pc.wrapping_add(1), zp_base),
            ];
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);

            let result = mode_zpy(&mut cpu);
            let zp_addr = zp_base.wrapping_add(y);
            let expected_addr = u16::from_le_bytes([zp_addr, 0x00]);

            prop_assert_eq!(result, Operand::Address(expected_addr));
            // Should still be in zero page after wrapping
            prop_assert!(expected_addr < 0x0100);
        }
    }

    // REGISTER INDEPENDENCE PROPTESTS
    proptest! {
        #[test]
        fn test_addressing_modes_dont_modify_registers(
            initial_pc in safe_pc_2bytes(),
            a in any::<u8>(),
            x in any::<u8>(),
            y in any::<u8>(),
        ) {
            // Set up memory with test values (only in valid WRAM range)
            let memory: Vec<(u16, u8)> = (0..=0xFF)
                .map(|i| (i as u16, (i as u8).wrapping_mul(7)))
                .collect();

            // Test each mode doesn't modify A, X, Y
            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_abs(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_abx(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_aby(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_acc(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_imm(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_imp(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_rel(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_zp0(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_zpx(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_zpy(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_idx(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);

            let mut cpu = cpu_with_state(initial_pc, a, x, y, &memory);
            let _ = mode_idy(&mut cpu);
            prop_assert_eq!(cpu.a, a);
            prop_assert_eq!(cpu.x, x);
            prop_assert_eq!(cpu.y, y);
        }
    }
}