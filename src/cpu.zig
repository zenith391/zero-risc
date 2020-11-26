const std = @import("std");
const builtin = @import("builtin");
pub const arch = Arch.default();

pub const Arch = struct {
    extensions: [255]bool = [1]bool {false} ** 255,
    XLEN: type = u32,
    XLENi: type = i32,

    pub fn default() Arch {
        var a = Arch {};
        a.extensions['I'] = true;
        a.extensions['C'] = true;
        return a;
    }
};

pub const Memory = struct {
    readFn: fn(usize, usize) u8,
    writeFn: fn(usize, usize, u8) void,
    userData: usize,

    pub inline fn read(self: *Memory, address: usize) u8 {
        return self.readFn(self.userData, address);
    }

    pub fn readIntLittle(self: *Memory, comptime T: type, address: usize) T {
        const bytes = @divExact(@typeInfo(T).Int.bits, 8);
        var data: [bytes]u8 = undefined;
        var i: usize = 0;
        while (i < data.len) : (i += 1) {
            data[i] = self.read(address + i);
        }
        return std.mem.readIntLittle(T, &data);
    }

    pub inline fn write(self: *Memory, address: usize, value: u8) void {
        self.writeFn(self.userData, address, value);
    }

    fn readSlice(userdata: usize, addr: usize) u8 {
        const slice = @intToPtr(*[]u8, userdata);
        return slice.*[addr];
    }

    fn writeSlice(userdata: usize, addr: usize, value: u8) void {
        var slice = @intToPtr(*[]u8, userdata);
        slice.*[addr] = value;
    }

    pub fn fromSlice(slice: *[]u8) Memory {
        return .{
            .readFn = Memory.readSlice,
            .writeFn = Memory.writeSlice,
            .userData = @ptrToInt(slice)
        };
    }
};

pub const Hart = struct {
    x: [32]arch.XLEN = [1]arch.XLEN{0} ** 32,
    pc: arch.XLEN = 0,
    memory: Memory,

    inline fn set_reg(self: *Hart, idx: u5, value: arch.XLEN) void {
        if (idx != 0) self.x[idx] = value;
    }

    inline fn get_reg(self: *Hart, idx: u5) arch.XLEN {
        return if (idx == 0) 0 else self.x[idx];
    }

    inline fn seti_reg(self: *Hart, idx: u5, value: arch.XLENi) void {
        if (idx != 0) self.x[idx] = @bitCast(arch.XLEN, value);
    }

    inline fn geti_reg(self: *Hart, idx: u5) arch.XLENi {
        return if (idx == 0) 0 else @bitCast(arch.XLENi, self.x[idx]);
    }

    fn invalidInstruction(self: *Hart) void {
        @setCold(true);
        const instr = self.memory.readIntLittle(u32, self.pc);
        const opcode = @intCast(u7, instr & 0x7F);
        std.log.err("Invalid instruction : {x}\n\tOperation Code: {b}", .{instr, opcode});
    }

    pub fn cycle(self: *Hart) void {
        const instr = self.memory.readIntLittle(u32, self.pc);
        const opcode = @intCast(u7, instr & 0x7F);
        const rd = @intCast(u5, (instr & 0xF80) >> 7);

        // Was it a compressed instruction ('C' extension) ? If yes, increment pc by 4, otherwise by 2
        var compressed: bool = false;

        // Was a jump taken ? If yes, do not increment pc
        var jumped: bool = false;

        //std.log.debug("instr = {x}", .{instr});
        //std.log.debug("opcode = {b}, pc = {x}", .{opcode, self.pc});

        if (opcode == 0b0010011) { // OP-IMM
            // I-type format
            const imm = @bitCast(i12, @intCast(u12, instr >> 20));
            const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
            const funct = (instr >> 12) & 0b111;

            if (funct == 0) { // ADDI
                std.log.debug("ADDI x{}, x{}, {}", .{rd, rs1, imm});
                self.seti_reg(rd, self.geti_reg(rs1) +% imm);
            } else if (funct == 7) { // ANDI
                std.log.debug("ANDI x{}, x{}, 0x{x}", .{rd, rs1, imm});
                self.set_reg(rd, self.get_reg(rs1) & @bitCast(u12, imm));
            }
        } else if (opcode == 0b0000011) { // LOAD
            // I-type format
            const offset = instr >> 20;
            const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
            const funct = (instr >> 12) & 0b111;
            const addr = self.get_reg(rs1) + offset;
            if (funct == 4) { // LBU
                std.log.debug("LBU x{}, x{}, 0x{x}", .{rd, rs1, offset});
                self.set_reg(rd, self.memory.read(addr));
            } else {
                self.invalidInstruction();
            }
        } else if (opcode == 0b0100011) { // STORE
            const funct = (instr >> 12) & 0b111;
            const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
            const rs2 = @intCast(u5, (instr >> 20) & 0b11111);
            const imm11_5 = (instr >> 25) & 0b1111111;
            const imm4_0 = (instr >> 7) & 0b11111;
            const offset = (imm11_5 << 5) | imm4_0;
            const addr = self.get_reg(rs1) + offset;

            if (funct == 0) { // SB
                std.log.debug("SB x{}, x{}, 0x{x}", .{rs1, rs2, offset});
                self.memory.write(addr, @intCast(u8, self.get_reg(rs2) & 0xFF));
            } else {
                self.invalidInstruction();
            }
        } else if (opcode == 0b1100011) { // BRANCH
            const funct = (instr >> 12) & 0b111;
            const rs1 = self.get_reg(@intCast(u5, (instr >> 15) & 0b11111));
            const rs2 = self.get_reg(@intCast(u5, (instr >> 20) & 0b11111));
            const imm11 = (instr >> 6) & 0b1;
            const imm12 = (instr >> 30) & 0b1;
            const imm4_1 = (instr >> 8) & 0b1111;
            const imm10_5 = (instr >> 25) & 0b111111;
            const offset = @bitCast(i12, @intCast(u12, (imm12 << 11) | (imm11 << 10) | (imm10_5 << 4) | imm4_1));
            if (funct == 0b001) { // BNE
                std.log.debug("BNE x{}, x{}, {}", .{rs1, rs2, offset});
                if (rs1 != rs2) {
                    self.pc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, offset)*2);
                    jumped = true;
                }
            } else if (funct == 0b000) { // BEQ
                std.log.debug("BEQ x{}, x{}, {}", .{rs1, rs2, offset});
                if (rs1 == rs2) {
                    self.pc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, offset)*2);
                    jumped = true;
                }
            } else {
                self.invalidInstruction();
            }
        } else if ((opcode & 0x3) == 0b01 and arch.extensions['C']) { // compressed instructions ('C' extension)
            const funct = (instr >> 13) & 0b111;
            if (funct == 0b000) { // C.ADDI
                const imm5 = (instr >> 12) & 0b1;
                const imm = (instr >> 2) & 0b11111 + (imm5 << 5);
                std.log.debug("C.ADDI x{}, x{}, {}", .{rd, rd, imm});
                self.set_reg(rd, self.get_reg(rd) + imm);
            } else if (funct == 0b101) { // C.J
                const imm11 = (instr >> 12) & 0b1;
                const imm4 = (instr >> 11) & 0b1;
                const imm98 = (instr >> 9) & 0b11;
                const imm10 = (instr >> 8) & 0b1;
                const imm6 = (instr >> 7) & 0b1;
                const imm7 = (instr >> 6) & 0b1;
                const imm31 = (instr >> 3) & 0b111;
                const imm5 = (instr >> 2) & 0b1;

                const imm = @bitCast(i12, @intCast(u12, (imm11 << 10) | (imm10 << 9) | (imm98 << 7) |
                    (imm7 << 6) | (imm6 << 5) | (imm5 << 4) | (imm4 << 3) | imm31));
                std.log.debug("C.J {}", .{imm});
                self.pc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, imm));
                jumped = true;
            } else {
                std.log.err("Unknown compressed opcode, funct = {b}", .{funct});
                self.invalidInstruction();
            }
            compressed = true;
        } else if (opcode == 0b1101111) { // JAL
            const imm10_1 = (instr >> 21) & 0b1111111111;
            const imm11 = (instr >> 19) & 0b1;
            const imm19_12 = (instr >> 12) & 0b11111111;
            const imm20 = (instr >> 30) & 0b1;
            const imm = @bitCast(i21, @intCast(u21, (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1)));
            std.log.debug("JAL {}", .{imm});
            self.set_reg(rd, self.pc + 4);
            self.pc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, imm));
            jumped = true;
        } else if (opcode == 0b0010111) { // AUIPC
            // U-type format
            const offset = @bitCast(i32, @intCast(u32, instr & 0xFFFFF000));
            std.log.debug("AUIPC x{}, {}", .{rd, offset});
            self.seti_reg(rd, @bitCast(arch.XLENi, self.pc) +% offset);
        } else {
            self.invalidInstruction();
        }

        self.pc += if (jumped) @as(arch.XLEN, 0) else if (compressed) @as(arch.XLEN, 2) else @as(arch.XLEN, 4);
    }
};