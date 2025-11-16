use std::ops::{Index, IndexMut};
use crate::bus::Bus;
use crate::cpu::addressing::{AddrModeFn, Operand};
use crate::{MemRead, MemWrite};

mod addressing;

#[derive(Clone, Copy)]
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

    fn step(&mut self) {

    }

    fn exec(&mut self, instruction: u8) {
        match instruction {
            // LDA
            0xA1 => self.ld(addressing::mode_idx, Register8::A),    // LDA, IDX
            0xA5 => self.ld(addressing::mode_zp0, Register8::A),    // LDA, ZP0
            0xA9 => self.ld(addressing::mode_imm, Register8::A),    // LDA, IMM
            0xAD => self.ld(addressing::mode_abs, Register8::A),    // LDA, ABS
            0xB1 => self.ld(addressing::mode_idy, Register8::A),    // LDA, IDY
            0xB5 => self.ld(addressing::mode_zpx, Register8::A),    // LDA, ZPX
            0xB9 => self.ld(addressing::mode_aby, Register8::A),    // LDA, ABY
            0xBD => self.ld(addressing::mode_abx, Register8::A),    // LDA, ABX

            // LDX
            0xA2 => self.ld(addressing::mode_imm, Register8::X),    // LDX, IMM
            0xA6 => self.ld(addressing::mode_zp0, Register8::X),    // LDX, ZP0
            0xAE => self.ld(addressing::mode_abs, Register8::X),    // LDX, ABS
            0xB6 => self.ld(addressing::mode_zpy, Register8::X),    // LDX, ZPY
            0xBE => self.ld(addressing::mode_aby, Register8::X),    // LDX, ABY

            // Not implemented
            _ => todo!("Instruction 0x{:04x} not implemented", instruction),
        }
    }

    fn update_zn_flags(&mut self, value: u8) {
        // Update Z flag: Check if the value was 0
        if value == 0 {
            self.status |= 0b0000_0010;  // Set bit 1
        } else {
            self.status &= 0b1111_1101;  // Clear bit 1
        }

        // Update N flag: Check if the most significant bit of the value was set
        if value & 0b1000_0000 != 0 {
            self.status |= 0b1000_0000;  // Set bit 7
        } else {
            self.status &= 0b0111_1111;  // Clear bit 7
        }
    }

    fn ld(&mut self, mode: AddrModeFn, reg: Register8) {
        match mode(self) {
            Operand::Value(val) => self[reg] = val,
            Operand::Address(addr) => self[reg] = self.read(addr),
        }
        self.update_zn_flags(self[reg]);
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

#[cfg(test)]
mod lda_instruction_tests {
    use super::*;

    // Helper to create a CPU with memory initialized
    fn setup_cpu(memory: &[(u16, u8)]) -> Cpu {
        let mut cpu = Cpu::new();
        for &(addr, val) in memory {
            if addr <= 0x07FF {
                cpu.write(addr, val);
            }
        }
        cpu
    }

    // Helper to check flag state
    fn check_flags(cpu: &Cpu, z_expected: bool, n_expected: bool) -> (bool, bool) {
        let z_actual = (cpu.status & 0b0000_0010) != 0;
        let n_actual = (cpu.status & 0b1000_0000) != 0;
        (z_actual == z_expected, n_actual == n_expected)
    }

    // LDA IMMEDIATE TESTS
    #[test]
    fn test_lda_imm_loads_value() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA9), // LDA immediate
            (0x0001, 0x42), // value to load
        ]);
        cpu.pc = 0x0000;

        cpu.exec(0xA9);

        assert_eq!(cpu.a, 0x42);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_lda_imm_zero_sets_z_flag() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA9),
            (0x0001, 0x00),
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00; // Clear all flags

        cpu.exec(0xA9);

        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0010, "Z flag should be set");
        assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000, "N flag should be clear");
    }

    #[test]
    fn test_lda_imm_negative_sets_n_flag() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA9),
            (0x0001, 0x80), // Negative value (bit 7 set)
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00;

        cpu.exec(0xA9);

        assert_eq!(cpu.a, 0x80);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000, "Z flag should be clear");
        assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000, "N flag should be set");
    }

    #[test]
    fn test_lda_imm_positive_clears_both_flags() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA9),
            (0x0001, 0x7F), // Positive value
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0xFF; // Set all flags initially

        cpu.exec(0xA9);

        assert_eq!(cpu.a, 0x7F);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000, "Z flag should be clear");
        assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000, "N flag should be clear");
    }

    // LDA ZERO-PAGE TESTS
    #[test]
    fn test_lda_zp0_loads_from_zero_page() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA5), // LDA zero-page
            (0x0001, 0x42), // Zero-page address
            (0x0042, 0x99), // Value at $0042
        ]);
        cpu.pc = 0x0000;

        cpu.exec(0xA5);

        assert_eq!(cpu.a, 0x99);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_lda_zp0_sets_flags_correctly() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA5),
            (0x0001, 0x50),
            (0x0050, 0x00), // Zero value
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00;

        cpu.exec(0xA5);

        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0010, "Z flag should be set");
    }

    // LDA ZERO-PAGE,X TESTS
    #[test]
    fn test_lda_zpx_loads_with_x_offset() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB5), // LDA zero-page,X
            (0x0001, 0x40), // Base address
            (0x0045, 0xAB), // Value at $0040 + $05 = $0045
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0x05;

        cpu.exec(0xB5);

        assert_eq!(cpu.a, 0xAB);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_lda_zpx_wraps_in_zero_page() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB5),
            (0x0001, 0xFF), // Base address
            (0x0004, 0xCD), // Value at ($FF + $05) & $FF = $04
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0x05;

        cpu.exec(0xB5);

        assert_eq!(cpu.a, 0xCD);
    }

    // LDA ABSOLUTE TESTS
    #[test]
    fn test_lda_abs_loads_from_absolute_address() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xAD), // LDA absolute
            (0x0001, 0x34), // Low byte of address
            (0x0002, 0x02), // High byte of address ($0234)
            (0x0234, 0x77), // Value at $0234
        ]);
        cpu.pc = 0x0000;

        cpu.exec(0xAD);

        assert_eq!(cpu.a, 0x77);
        assert_eq!(cpu.pc, 0x0003);
    }

    #[test]
    fn test_lda_abs_negative_value() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xAD),
            (0x0001, 0x50),
            (0x0002, 0x00),
            (0x0050, 0xFF), // Negative value
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00;

        cpu.exec(0xAD);

        assert_eq!(cpu.a, 0xFF);
        assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000, "N flag should be set");
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000, "Z flag should be clear");
    }

    // LDA ABSOLUTE,X TESTS
    #[test]
    fn test_lda_abx_loads_with_x_offset() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xBD), // LDA absolute,X
            (0x0001, 0x00),
            (0x0002, 0x02), // Base address $0200
            (0x020A, 0x88), // Value at $0200 + $0A = $020A
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0x0A;

        cpu.exec(0xBD);

        assert_eq!(cpu.a, 0x88);
        assert_eq!(cpu.pc, 0x0003);
    }

    // LDA ABSOLUTE,Y TESTS
    #[test]
    fn test_lda_aby_loads_with_y_offset() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB9), // LDA absolute,Y
            (0x0001, 0x00),
            (0x0002, 0x03), // Base address $0300
            (0x0315, 0x66), // Value at $0300 + $15 = $0315
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x15;

        cpu.exec(0xB9);

        assert_eq!(cpu.a, 0x66);
        assert_eq!(cpu.pc, 0x0003);
    }

    // LDA X-INDEXED, INDIRECT TESTS
    #[test]
    fn test_lda_idx_loads_via_indirect_pointer() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA1), // LDA (indirect,X)
            (0x0001, 0x20), // Zero-page base
            (0x0024, 0x74), // Pointer low byte at ($20 + $04)
            (0x0025, 0x00), // Pointer high byte
            (0x0074, 0x33), // Value at target address
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0x04;

        cpu.exec(0xA1);

        assert_eq!(cpu.a, 0x33);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_lda_idx_wraps_zero_page() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA1),
            (0x0001, 0xFF), // Zero-page base
            (0x0002, 0x50), // Pointer low byte at ($FF + $03) & $FF = $02
            (0x0003, 0x00), // Pointer high byte at $03
            (0x0050, 0xAA), // Value at target address
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0x03;

        cpu.exec(0xA1);

        assert_eq!(cpu.a, 0xAA);
    }

    // LDA INDIRECT, Y-INDEXED TESTS
    #[test]
    fn test_lda_idy_loads_via_indirect_pointer_plus_y() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB1), // LDA (indirect),Y
            (0x0001, 0x30), // Zero-page address
            (0x0030, 0x00), // Pointer low byte
            (0x0031, 0x02), // Pointer high byte ($0200)
            (0x020A, 0x55), // Value at $0200 + $0A = $020A
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x0A;

        cpu.exec(0xB1);

        assert_eq!(cpu.a, 0x55);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_lda_idy_zero_page_pointer_wraps() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB1),
            (0x0001, 0xFF), // Zero-page address $FF
            (0x00FF, 0x00), // Pointer low byte
            (0x0000, 0x01), // Pointer high byte (wraps to $00) -> $0100
            (0x0105, 0x99), // Value at $0100 + $05 = $0105
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x05;

        cpu.exec(0xB1);

        assert_eq!(cpu.a, 0x99);
    }

    // FLAG BEHAVIOR COMPREHENSIVE TESTS
    #[test]
    fn test_lda_flag_combinations() {
        // Test various value ranges
        let test_cases = vec![
            (0x00, true, false),   // Zero: Z=1, N=0
            (0x01, false, false),  // Small positive: Z=0, N=0
            (0x7F, false, false),  // Max positive: Z=0, N=0
            (0x80, false, true),   // Min negative: Z=0, N=1
            (0xFF, false, true),   // Max negative: Z=0, N=1
        ];

        for (value, expect_z, expect_n) in test_cases {
            let mut cpu = setup_cpu(&[
                (0x0000, 0xA9),
                (0x0001, value),
            ]);
            cpu.pc = 0x0000;
            cpu.status = 0x00;

            cpu.exec(0xA9);

            assert_eq!(cpu.a, value, "Failed for value 0x{:02X}", value);

            let z_flag = (cpu.status & 0b0000_0010) != 0;
            let n_flag = (cpu.status & 0b1000_0000) != 0;

            assert_eq!(z_flag, expect_z,
                       "Z flag mismatch for value 0x{:02X}: expected {}, got {}",
                       value, expect_z, z_flag);
            assert_eq!(n_flag, expect_n,
                       "N flag mismatch for value 0x{:02X}: expected {}, got {}",
                       value, expect_n, n_flag);
        }
    }

    // REGISTER PRESERVATION TESTS
    #[test]
    fn test_lda_preserves_x_and_y() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA9),
            (0x0001, 0x42),
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0xAA;
        cpu.y = 0xBB;

        cpu.exec(0xA9);

        assert_eq!(cpu.x, 0xAA, "X register should be preserved");
        assert_eq!(cpu.y, 0xBB, "Y register should be preserved");
    }

    #[test]
    fn test_lda_only_affects_zn_flags() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA9),
            (0x0001, 0x42),
        ]);
        cpu.pc = 0x0000;
        // Set all other flags (C, I, D, V)
        cpu.status = 0b0111_1101; // All flags except Z and N

        cpu.exec(0xA9);

        // Check that only Z and N flags can change, others preserved
        let other_flags = cpu.status & 0b0111_1101;
        assert_eq!(other_flags, 0b0111_1101,
                   "LDA should only modify Z and N flags, not C, I, D, or V");
    }

    // PC ADVANCEMENT TESTS
    #[test]
    fn test_lda_pc_advancement() {
        let test_cases = vec![
            (0xA9, 0x0000, 0x0002), // Immediate: PC + 2
            (0xA5, 0x0000, 0x0002), // Zero-page: PC + 2
            (0xB5, 0x0000, 0x0002), // Zero-page,X: PC + 2
            (0xAD, 0x0000, 0x0003), // Absolute: PC + 3
            (0xBD, 0x0000, 0x0003), // Absolute,X: PC + 3
            (0xB9, 0x0000, 0x0003), // Absolute,Y: PC + 3
            (0xA1, 0x0000, 0x0002), // (Indirect,X): PC + 2
            (0xB1, 0x0000, 0x0002), // (Indirect),Y: PC + 2
        ];

        for (opcode, start_pc, expected_pc) in test_cases {
            let mut cpu = setup_cpu(&[
                (start_pc, opcode),
                (start_pc + 1, 0x10), // Operand byte 1
                (start_pc + 2, 0x02), // Operand byte 2 (for 3-byte instructions) -> $0210
                (0x0010, 0x42),       // Some data at zero-page $10
                (0x0011, 0x00),       // Pointer low byte for indirect modes
                (0x0012, 0x02),       // Pointer high byte for indirect modes -> $0200
                (0x0042, 0x99),       // Data at $0042
                (0x0210, 0xAA),       // Data at $0210 for absolute modes
                (0x0200, 0xBB),       // Data at $0200 for indirect modes
            ]);
            cpu.pc = start_pc;
            cpu.x = 0x02;
            cpu.y = 0x02;

            cpu.exec(opcode);

            assert_eq!(cpu.pc, expected_pc,
                       "PC advancement failed for opcode 0x{:02X}", opcode);
        }
    }
}

#[cfg(test)]
mod ldx_instruction_tests {
    use super::*;

    // Helper to create a CPU with memory initialized
    fn setup_cpu(memory: &[(u16, u8)]) -> Cpu {
        let mut cpu = Cpu::new();
        for &(addr, val) in memory {
            if addr <= 0x07FF {
                cpu.write(addr, val);
            }
        }
        cpu
    }

    // LDX IMMEDIATE TESTS
    #[test]
    fn test_ldx_imm_loads_value() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA2), // LDX immediate
            (0x0001, 0x42), // value to load
        ]);
        cpu.pc = 0x0000;

        cpu.exec(0xA2);

        assert_eq!(cpu.x, 0x42);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_ldx_imm_zero_sets_z_flag() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA2),
            (0x0001, 0x00),
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00;

        cpu.exec(0xA2);

        assert_eq!(cpu.x, 0x00);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0010, "Z flag should be set");
        assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000, "N flag should be clear");
    }

    #[test]
    fn test_ldx_imm_negative_sets_n_flag() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA2),
            (0x0001, 0x80),
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00;

        cpu.exec(0xA2);

        assert_eq!(cpu.x, 0x80);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000, "Z flag should be clear");
        assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000, "N flag should be set");
    }

    #[test]
    fn test_ldx_imm_positive_clears_both_flags() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA2),
            (0x0001, 0x7F),
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0xFF; // Set all flags initially

        cpu.exec(0xA2);

        assert_eq!(cpu.x, 0x7F);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000, "Z flag should be clear");
        assert_eq!(cpu.status & 0b1000_0000, 0b0000_0000, "N flag should be clear");
    }

    // LDX ZERO-PAGE TESTS
    #[test]
    fn test_ldx_zp0_loads_from_zero_page() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA6), // LDX zero-page
            (0x0001, 0x42), // Zero-page address
            (0x0042, 0x99), // Value at $0042
        ]);
        cpu.pc = 0x0000;

        cpu.exec(0xA6);

        assert_eq!(cpu.x, 0x99);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_ldx_zp0_sets_flags_correctly() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA6),
            (0x0001, 0x50),
            (0x0050, 0x00),
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00;

        cpu.exec(0xA6);

        assert_eq!(cpu.x, 0x00);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0010, "Z flag should be set");
    }

    // LDX ZERO-PAGE,Y TESTS
    #[test]
    fn test_ldx_zpy_loads_with_y_offset() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB6), // LDX zero-page,Y
            (0x0001, 0x40), // Base address
            (0x0045, 0xAB), // Value at $0040 + $05 = $0045
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x05;

        cpu.exec(0xB6);

        assert_eq!(cpu.x, 0xAB);
        assert_eq!(cpu.pc, 0x0002);
    }

    #[test]
    fn test_ldx_zpy_wraps_in_zero_page() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB6),
            (0x0001, 0xFF), // Base address
            (0x0004, 0xCD), // Value at ($FF + $05) & $FF = $04
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x05;

        cpu.exec(0xB6);

        assert_eq!(cpu.x, 0xCD);
    }

    #[test]
    fn test_ldx_zpy_negative_value() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB6),
            (0x0001, 0x10),
            (0x0015, 0xFF),
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x05;
        cpu.status = 0x00;

        cpu.exec(0xB6);

        assert_eq!(cpu.x, 0xFF);
        assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000, "N flag should be set");
    }

    // LDX ABSOLUTE TESTS
    #[test]
    fn test_ldx_abs_loads_from_absolute_address() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xAE), // LDX absolute
            (0x0001, 0x34), // Low byte
            (0x0002, 0x02), // High byte ($0234)
            (0x0234, 0x77), // Value at $0234
        ]);
        cpu.pc = 0x0000;

        cpu.exec(0xAE);

        assert_eq!(cpu.x, 0x77);
        assert_eq!(cpu.pc, 0x0003);
    }

    #[test]
    fn test_ldx_abs_negative_value() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xAE),
            (0x0001, 0x50),
            (0x0002, 0x00),
            (0x0050, 0xFF),
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0x00;

        cpu.exec(0xAE);

        assert_eq!(cpu.x, 0xFF);
        assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000, "N flag should be set");
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0000, "Z flag should be clear");
    }

    // LDX ABSOLUTE,Y TESTS
    #[test]
    fn test_ldx_aby_loads_with_y_offset() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xBE), // LDX absolute,Y
            (0x0001, 0x00),
            (0x0002, 0x02), // Base address $0200
            (0x020A, 0x88), // Value at $0200 + $0A = $020A
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x0A;

        cpu.exec(0xBE);

        assert_eq!(cpu.x, 0x88);
        assert_eq!(cpu.pc, 0x0003);
    }

    #[test]
    fn test_ldx_aby_zero_value() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xBE),
            (0x0001, 0x00),
            (0x0002, 0x03), // Base address $0300
            (0x0315, 0x00), // Value at $0300 + $15 = $0315
        ]);
        cpu.pc = 0x0000;
        cpu.y = 0x15;
        cpu.status = 0x00;

        cpu.exec(0xBE);

        assert_eq!(cpu.x, 0x00);
        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0010, "Z flag should be set");
    }

    // FLAG BEHAVIOR COMPREHENSIVE TESTS
    #[test]
    fn test_ldx_flag_combinations() {
        let test_cases = vec![
            (0x00, true, false),   // Zero: Z=1, N=0
            (0x01, false, false),  // Small positive: Z=0, N=0
            (0x7F, false, false),  // Max positive: Z=0, N=0
            (0x80, false, true),   // Min negative: Z=0, N=1
            (0xFF, false, true),   // Max negative: Z=0, N=1
        ];

        for (value, expect_z, expect_n) in test_cases {
            let mut cpu = setup_cpu(&[
                (0x0000, 0xA2),
                (0x0001, value),
            ]);
            cpu.pc = 0x0000;
            cpu.status = 0x00;

            cpu.exec(0xA2);

            assert_eq!(cpu.x, value, "Failed for value 0x{:02X}", value);

            let z_flag = (cpu.status & 0b0000_0010) != 0;
            let n_flag = (cpu.status & 0b1000_0000) != 0;

            assert_eq!(z_flag, expect_z,
                       "Z flag mismatch for value 0x{:02X}: expected {}, got {}",
                       value, expect_z, z_flag);
            assert_eq!(n_flag, expect_n,
                       "N flag mismatch for value 0x{:02X}: expected {}, got {}",
                       value, expect_n, n_flag);
        }
    }

    // REGISTER PRESERVATION TESTS
    #[test]
    fn test_ldx_preserves_a_and_y() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA2),
            (0x0001, 0x42),
        ]);
        cpu.pc = 0x0000;
        cpu.a = 0xAA;
        cpu.y = 0xBB;

        cpu.exec(0xA2);

        assert_eq!(cpu.a, 0xAA, "A register should be preserved");
        assert_eq!(cpu.y, 0xBB, "Y register should be preserved");
    }

    #[test]
    fn test_ldx_only_affects_zn_flags() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA2),
            (0x0001, 0x42),
        ]);
        cpu.pc = 0x0000;
        cpu.status = 0b0111_1101; // All flags except Z and N

        cpu.exec(0xA2);

        let other_flags = cpu.status & 0b0111_1101;
        assert_eq!(other_flags, 0b0111_1101,
                   "LDX should only modify Z and N flags, not C, I, D, or V");
    }

    // PC ADVANCEMENT TESTS
    #[test]
    fn test_ldx_pc_advancement() {
        let test_cases = vec![
            (0xA2, 0x0000, 0x0002), // Immediate: PC + 2
            (0xA6, 0x0000, 0x0002), // Zero-page: PC + 2
            (0xB6, 0x0000, 0x0002), // Zero-page,Y: PC + 2
            (0xAE, 0x0000, 0x0003), // Absolute: PC + 3
            (0xBE, 0x0000, 0x0003), // Absolute,Y: PC + 3
        ];

        for (opcode, start_pc, expected_pc) in test_cases {
            let mut cpu = setup_cpu(&[
                (start_pc, opcode),
                (start_pc + 1, 0x10),
                (start_pc + 2, 0x02), // -> $0210
                (0x0010, 0x42),
                (0x0012, 0x99),
                (0x0210, 0xAA),
            ]);
            cpu.pc = start_pc;
            cpu.y = 0x02;

            cpu.exec(opcode);

            assert_eq!(cpu.pc, expected_pc,
                       "PC advancement failed for opcode 0x{:02X}", opcode);
        }
    }

    // SPECIAL: Y-INDEXING BEHAVIOR
    #[test]
    fn test_ldx_uses_y_register_not_x() {
        // This test verifies that LDX correctly uses Y for indexing, not X
        let mut cpu = setup_cpu(&[
            (0x0000, 0xB6), // LDX zero-page,Y
            (0x0001, 0x20),
            (0x0025, 0x55), // At $20 + Y($05) = $25
            (0x0028, 0x88), // At $20 + X($08) = $28
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0x08; // Should NOT be used
        cpu.y = 0x05; // Should be used

        cpu.exec(0xB6);

        // Should load from $0025 (using Y), not $0028 (using X)
        assert_eq!(cpu.x, 0x55, "LDX should use Y register for indexing");
    }

    #[test]
    fn test_ldx_aby_uses_y_not_x() {
        let mut cpu = setup_cpu(&[
            (0x0000, 0xBE), // LDX absolute,Y
            (0x0001, 0x00),
            (0x0002, 0x02), // Base $0200
            (0x0205, 0x55), // At $0200 + Y($05) = $0205
            (0x0208, 0x88), // At $0200 + X($08) = $0208
        ]);
        cpu.pc = 0x0000;
        cpu.x = 0x08; // Should NOT be used
        cpu.y = 0x05; // Should be used

        cpu.exec(0xBE);

        assert_eq!(cpu.x, 0x55, "LDX absolute,Y should use Y register");
    }

    // CROSS-VALIDATION WITH LDA
    #[test]
    fn test_ldx_behaves_like_lda_for_flags() {
        // LDX should set flags identically to LDA
        let values = vec![0x00, 0x42, 0x80, 0xFF];

        for value in values {
            // Test LDA
            let mut cpu_lda = setup_cpu(&[
                (0x0000, 0xA9), // LDA immediate
                (0x0001, value),
            ]);
            cpu_lda.pc = 0x0000;
            cpu_lda.status = 0x00;
            cpu_lda.exec(0xA9);
            let lda_flags = cpu_lda.status & 0b1000_0010; // Z and N flags

            // Test LDX
            let mut cpu_ldx = setup_cpu(&[
                (0x0000, 0xA2), // LDX immediate
                (0x0001, value),
            ]);
            cpu_ldx.pc = 0x0000;
            cpu_ldx.status = 0x00;
            cpu_ldx.exec(0xA2);
            let ldx_flags = cpu_ldx.status & 0b1000_0010;

            assert_eq!(lda_flags, ldx_flags,
                       "LDX and LDA should set flags identically for value 0x{:02X}", value);
        }
    }

    // EDGE CASE: LOADING INTO X THEN USING X
    #[test]
    fn test_ldx_then_use_x_for_indexing() {
        // Load a value into X, then use it for indexing
        let mut cpu = setup_cpu(&[
            (0x0000, 0xA2), // LDX immediate
            (0x0001, 0x05), // Load 5 into X
            (0x0002, 0xB5), // LDA zero-page,X
            (0x0003, 0x20), // Base address $20
            (0x0025, 0x99), // Value at $20 + $05 = $25
        ]);
        cpu.pc = 0x0000;

        cpu.exec(0xA2); // LDX #$05
        assert_eq!(cpu.x, 0x05);
        assert_eq!(cpu.pc, 0x0002);

        cpu.exec(0xB5); // LDA $20,X
        assert_eq!(cpu.a, 0x99, "Should load from $0025 using X=$05");
    }
}