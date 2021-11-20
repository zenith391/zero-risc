const std = @import("std");
const builtin = @import("builtin");
const riscv = std.Target.riscv;
pub const arch = Arch.default();

pub const Arch = struct {
    features: std.Target.Cpu.Feature.Set,
    XLEN: type = u32,
    XLENi: type = i32,
    XLENd: type = u64,
    XLENb: u8 = 32,
    vendorId: u32 = 0,
    archId: u32 = 0xF0000000,
    implementationId: u32 = 0,

    pub fn default() Arch {
        var a = Arch {
            .features = riscv.featureSet(&[_]riscv.Feature{ .c, .m })
        };
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

    pub inline fn readi(self: *Memory, address: usize) i8 {
        return @bitCast(i8, self.readFn(self.userData, address));
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

    pub fn writeIntLittle(self: *Memory, comptime T: type, address: usize, value: T) void {
        const bytes = @divExact(@typeInfo(T).Int.bits, 8);
        var data: [bytes]u8 = undefined;
        std.mem.writeIntLittle(T, &data, value);
        for (data) |byte, i| {
            self.write(address + i, byte);
        }
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
    /// x0-31 registers
    x: [32]arch.XLEN = [1]arch.XLEN{0} ** 32,
    csr: [4096]arch.XLEN = [1]arch.XLEN{0} ** 4096,
    ecallFn: ?fn(hart: *Hart) void = null,
    pc: arch.XLEN = 0,
    memory: Memory,

    pub fn init(memory: Memory, pc: arch.XLEN, hartId: arch.XLEN) Hart {
        var hart = Hart {
            .memory = memory,
            .pc = pc
        };
        hart.csr[0xF11] = arch.vendorId;
        hart.csr[0xF12] = arch.archId;
        hart.csr[0xF13] = arch.implementationId;
        hart.csr[0xF14] = hartId;
        // TODO: set misa, mstatus
        return hart;
    }

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
        std.log.err("Invalid instruction : {x}\n\tOperation Code: {b}\n\tPC: 0x{x}", .{instr, opcode, self.pc});
        std.process.exit(1);
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
        std.log.debug("opcode = {b}, pc = {x}", .{opcode, self.pc});

        switch (opcode) {
            0b0010011 => { // OP-IMM
                // I-type format
                const imm = @bitCast(i12, @intCast(u12, instr >> 20));
                const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
                const funct = (instr >> 12) & 0b111;

                if (funct == 0b000) { // ADDI
                    std.log.debug("ADDI x{}, x{}, {}", .{rd, rs1, imm});
                    self.seti_reg(rd, self.geti_reg(rs1) +% imm);
                } else if (funct == 0b001) { // SLLI
                    if ((@bitCast(u12, imm) & 0x800) == 0) { // SLLI
                        std.log.debug("SLLI x{}, x{}, {}", .{rd, rs1, imm});
                        self.set_reg(rd, self.get_reg(rs1) << @intCast(u5, imm));
                    } else {
                        self.invalidInstruction();
                    }
                } else if (funct == 0b011) { // SLTIU
                    std.log.debug("SLTIU x{}, x{}, {}", .{rd, rs1, imm});
                    const immU = @bitCast(u12, imm);
                    self.set_reg(rd, if (self.get_reg(rs1) < immU) 1 else 0);
                } else if (funct == 0b100) { // XORI
                    std.log.debug("XORI x{}, x{}, 0x{x}", .{rd, rs1, imm});
                    self.set_reg(rd, self.get_reg(rs1) ^ @bitCast(u12, imm));
                } else if (funct == 0b101) { // SRLI / SRAI
                    if ((@bitCast(u12, imm) & 0x400) == 0) { // SRLI
                        std.log.debug("SRLI x{}, x{}, {}", .{rd, rs1, imm});
                        self.set_reg(rd, self.get_reg(rs1) >> @intCast(u5, imm));
                    } else {
                        std.log.debug("SRAI x{}, x{}, {}", .{rd, rs1, imm});
                        const shift = @truncate(u5, instr >> 20);
                        const val = self.geti_reg(rs1);
                        if (val < 0) {
                            //std.log.notice("val < 0 = {} = {}", .{val, @bitCast(u32, val)});
                            self.set_reg(rd, ~(~(self.get_reg(rs1)) >> shift));
                        } else {
                            self.seti_reg(rd, val >> shift);
                        }
                    }
                } else if (funct == 0b110) { // ORI
                    const immU = @bitCast(u32, @intCast(i32, imm));
                    std.log.debug("ORI x{}, x{}, 0x{x}", .{rd, rs1, immU});
                    self.set_reg(rd, self.get_reg(rs1) | immU);
                } else if (funct == 0b111) { // ANDI
                    const immU = @bitCast(u32, @intCast(i32, imm));
                    std.log.debug("ANDI x{}, x{}, 0x{x}", .{rd, rs1, immU});
                    self.set_reg(rd, self.get_reg(rs1) & immU);
                } else {
                    std.log.err("OP-IMM funct = {b}", .{funct});
                    self.invalidInstruction();
                }
            },
            0b0011011 => { // OP-IMM-32
                const imm = @bitCast(i12, @intCast(u12, instr >> 20));
                const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
                const funct = (instr >> 12) & 0b111;

                if (funct == 0b000) { // ADDIW
                    std.log.debug("ADDI x{}, x{}, {}", .{rd, rs1, imm});
                    self.seti_reg(rd, @truncate(i32, self.geti_reg(rs1)) +% imm);
                } else {
                    std.log.err("OP-IMM-32 funct = {b}", .{funct});
                    self.invalidInstruction();
                }
            },
            0b0110011 => { // OP
                const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
                const rs2 = @intCast(u5, (instr >> 20) & 0b11111);
                const funct = (((instr >> 25) & 0x7F) << 3) | ((instr >> 12) & 0b111);
                if (funct == 0b000) { // ADD
                    std.log.debug("ADD x{}, x{}, x{}", .{rd, rs1, rs2});
                    self.set_reg(rd, self.get_reg(rs1) +% self.get_reg(rs2));
                } else if (funct == 0b001) { // SLL
                    std.log.debug("SLL x{}, x{}, x{}", .{rd, rs1, rs2});
                    const shift = @truncate(u5, self.get_reg(rs2));
                    self.set_reg(rd, self.get_reg(rs1) << shift);
                } else if (funct == 0b011) { // SLTU
                    std.log.debug("SLTU x{}, x{}, x{}", .{rd, rs1, rs2});
                    self.set_reg(rd, if (self.get_reg(rs1) < self.get_reg(rs2)) 1 else 0);
                } else if (funct == 0b100) { // XOR
                    std.log.debug("XOR x{}, x{}, x{}", .{rd, rs1, rs2});
                    self.set_reg(rd, self.get_reg(rs1) ^ self.get_reg(rs2));
                } else if (funct == 0b101) { // SRL
                    std.log.debug("SRL x{}, x{}, x{}", .{rd, rs1, rs2});
                    const shift = @truncate(u5, self.get_reg(rs2));
                    self.set_reg(rd, self.get_reg(rs1) >> shift);
                } else if (funct == 0b110) { // OR
                    std.log.debug("OR x{}, x{}, x{}", .{rd, rs1, rs2});
                    self.set_reg(rd, self.get_reg(rs1) | self.get_reg(rs2));
                } else if (funct == 0b111) { // AND
                    std.log.debug("AND x{}, x{}, x{}", .{rd, rs1, rs2});
                    self.set_reg(rd, self.get_reg(rs1) & self.get_reg(rs2));
                } else if (comptime riscv.featureSetHas(arch.features, .m) and funct == 0b1000) { // MUL
                    std.log.debug("MUL x{}, x{}, x{}", .{rd, rs1, rs2});
                    self.set_reg(rd, self.get_reg(rs1) *% self.get_reg(rs2));
                } else if (comptime riscv.featureSetHas(arch.features, .m) and funct == 0b1011) { // MULHU
                    std.log.debug("MULHU x{}, x{}, x{}", .{rd, rs1, rs2});
                    const multiplicand = self.get_reg(rs1);
                    const multiplier = self.get_reg(rs2);
                    const result = @intCast(arch.XLEN, std.math.mulWide(arch.XLEN, multiplicand, multiplier) >> arch.XLENb);
                    self.set_reg(rd, result);
                } else if (comptime riscv.featureSetHas(arch.features, .m) and funct == 0b1101) { // DIVU
                    std.log.debug("DIVU x{}, x{}, x{}", .{rd, rs1, rs2});
                    const dividend = self.get_reg(rs1);
                    const divisor = self.get_reg(rs2);
                    self.set_reg(rd, dividend / divisor);
                } else if (comptime riscv.featureSetHas(arch.features, .m) and funct == 0b1111) { // REMU
                    std.log.debug("REMU x{}, x{}, x{}", .{rd, rs1, rs2});
                    const dividend = self.get_reg(rs1);
                    const divisor = self.get_reg(rs2);
                    self.set_reg(rd, dividend % divisor);
                } else if (funct == 0b0100000000) { // SUB
                    std.log.debug("SUB x{}, x{}, x{}", .{rd, rs1, rs2});
                    self.set_reg(rd, self.get_reg(rs1) -% self.get_reg(rs2));
                } else {
                    self.invalidInstruction();
                }
            },
            0b0000011 => { // LOAD
                // I-type format
                const offset = @bitCast(i12, @intCast(u12, instr >> 20));
                const rs1 = @truncate(u5, (instr >> 15));
                const funct = (instr >> 12) & 0b111;
                const addr = @bitCast(u32, self.geti_reg(rs1) +% offset);
                if (funct == 0) { // LB
                    std.log.debug("LB x{}, x{}, {}", .{rd, rs1, offset});
                    self.seti_reg(rd, @intCast(i32, self.memory.readi(addr)));
                } else if (funct == 1) { // LH
                    std.log.debug("LH x{}, x{}, {}", .{rd, rs1, offset});
                    self.seti_reg(rd, @intCast(i32, self.memory.readIntLittle(i16, addr)));
                } else if (funct == 2) { // LW
                    std.log.debug("LW x{}, x{}, {}", .{rd, rs1, offset});
                    self.set_reg(rd, self.memory.readIntLittle(u32, addr));
                } else if (funct == 4) { // LBU
                    std.log.debug("LBU x{}, x{}, 0x{x}", .{rd, rs1, offset});
                    self.set_reg(rd, self.memory.read(addr));
                } else if (funct == 5) { // LHU
                    std.log.debug("LHU x{}, x{}, 0x{x}", .{rd, rs1, offset});
                    self.set_reg(rd, self.memory.readIntLittle(u16, addr));
                } else {
                    self.invalidInstruction();
                }
            },
            0b0100011 => { // STORE
                const funct = (instr >> 12) & 0b111;
                const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
                const rs2 = @intCast(u5, (instr >> 20) & 0b11111);
                const imm11_5 = (instr >> 25) & 0b1111111;
                const imm4_0 = (instr >> 7) & 0b11111;
                const offset = @bitCast(i12, @intCast(u12, (imm11_5 << 5) | imm4_0));
                const addr = @bitCast(u32, @bitCast(i32, self.get_reg(rs1)) +% offset);

                if (funct == 0) { // SB
                    std.log.debug("SB {}(x{}), x{}", .{offset, rs1, rs2});
                    self.memory.write(addr, @intCast(u8, self.get_reg(rs2) & 0xFF));
                } else if (funct == 1) { // SH
                    std.log.debug("SH {}(x{}), x{}", .{offset, rs1, rs2});
                    self.memory.writeIntLittle(u16, addr, @intCast(u16, self.get_reg(rs2) & 0xFFFF));
                } else if (funct == 2) { // SW
                    std.log.debug("SW {}(x{}), x{}", .{offset, rs1, rs2});
                    self.memory.writeIntLittle(u32, addr, self.get_reg(rs2));
                } else {
                    self.invalidInstruction();
                }
            },
            0b1100011 => { // BRANCH
                const funct = (instr >> 12) & 0b111;
                const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
                const rs2 = @intCast(u5, (instr >> 20) & 0b11111);
                const imm11 = (instr >> 7) & 0b1;
                const imm12 = (instr >> 30) & 0b1;
                const imm4_1 = (instr >> 8) & 0b1111;
                const imm10_5 = (instr >> 25) & 0b111111;
                const offset = @bitCast(i13, @intCast(u13, (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1)));
                const newPc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, offset));

                if (funct == 0b000) { // BEQ
                    std.log.debug("BEQ x{}, x{}, 0x{x}", .{rs1, rs2, newPc});
                    if (self.get_reg(rs1) == self.get_reg(rs2)) {
                        self.pc = newPc;
                        jumped = true;
                    }
                } else if (funct == 0b001) { // BNE
                    std.log.debug("BNE x{}, x{}, 0x{x}", .{rs1, rs2, newPc});
                    if (self.get_reg(rs1) != self.get_reg(rs2)) {
                        self.pc = newPc;
                        jumped = true;
                    }
                } else if (funct == 0b100) { // BLT
                    std.log.debug("BLT x{}, x{}, 0x{x}", .{rs1, rs2, newPc});
                    if (self.geti_reg(rs1) < self.geti_reg(rs2)) {
                        self.pc = newPc;
                        jumped = true;
                    }
                } else if (funct == 0b101) { // BGE
                    std.log.debug("BGE x{}, x{}, 0x{x}", .{rs1, rs2, newPc});
                    if (self.geti_reg(rs1) > self.geti_reg(rs2)) {
                        self.pc = newPc;
                        jumped = true;
                    }
                } else if (funct == 0b110) { // BLTU
                    std.log.debug("BLTU x{}, x{}, 0x{x} ({} < {} ?)", .{rs1, rs2, newPc, self.get_reg(rs1), self.get_reg(rs2)});
                    if (self.get_reg(rs1) < self.get_reg(rs2)) {
                        self.pc = newPc;
                        jumped = true;
                    }
                } else if (funct == 0b111) { // BGEU
                    std.log.debug("BGEU x{}, x{}, 0x{x}", .{rs1, rs2, newPc});
                    if (self.get_reg(rs1) > self.get_reg(rs2)) {
                        self.pc = newPc;
                        jumped = true;
                    }
                } else {
                    self.invalidInstruction();
                }
            },
            0b1101111 => { // JAL
                const imm10_1 = (instr >> 21) & 0b1111111111;
                const imm11 = (instr >> 19) & 0b1;
                const imm19_12 = (instr >> 12) & 0b11111111;
                const imm20 = (instr >> 30) & 0b1;
                const imm = @bitCast(i21, @intCast(u21, (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1)));
                const newPc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, imm));
                std.log.debug("JAL x{}, 0x{x}", .{rd, imm});
                self.set_reg(rd, self.pc + 4);
                self.pc = newPc;
                jumped = true;
            },
            0b1100111 => { // JALR
                const imm = @bitCast(i12, @intCast(u12, (instr >> 20)));
                const rs1 = @intCast(u5, (instr >> 15) & 0b11111);
                const newPc = @intCast(u32, self.geti_reg(rs1) +% imm);
                std.log.debug("JALR x{}, {}(x{})", .{rd, imm, rs1});
                self.set_reg(rd, self.pc + 4);
                self.pc = newPc;
                jumped = true;
            },
            0b0010111 => { // AUIPC
                // U-type format
                const offset = @bitCast(i32, @intCast(u32, instr & 0xFFFFF000));
                std.log.debug("AUIPC x{}, {}", .{rd, offset});
                self.seti_reg(rd, @bitCast(arch.XLENi, self.pc) +% offset);
            },
            0b0110111 => { // LUI
                // U-type format
                const imm = @intCast(u32, instr & 0xFFFFF000);
                std.log.debug("LUI x{}, 0x{x}", .{rd, (imm>>12)});
                self.set_reg(rd, imm);
            },
            0b1110011 => { // SYSTEM
                const bit = @truncate(u1, (instr >> 20));
                if (bit == 0) { // ECALL
                    std.log.debug("ECALL", .{});
                    if (self.ecallFn) |func| {
                        func(self);
                    } else {
                        std.log.warn("Missing ECALL handler", .{});
                    }
                } else { // EBREAK
                    std.log.notice("EBREAK", .{});
                    @breakpoint(); // TODO: pause and print info on the emulated CPU
                }
            },
            else => {
                if (comptime riscv.featureSetHas(arch.features, .c) and (opcode & 0x3) == 0b01) {
                    const funct = (instr >> 13) & 0b111;
                    if (funct == 0b000) { // C.ADDI
                        const imm5 = (instr >> 12) & 0b1;
                        const imm = @bitCast(i6, @intCast(u6, ((instr >> 2) & 0b11111) + (imm5 << 5)));
                        std.log.debug("C.ADDI x{}, x{}, {}", .{rd, rd, imm});
                        self.seti_reg(rd, self.geti_reg(rd) +% imm);
                    } else if (funct == 0b010) { // C.LI
                        const imm40 = (instr >> 2) & 0b11111;
                        const imm5 = (instr >> 12) & 0b1;
                        const imm = @bitCast(i6, @intCast(u6, (imm5 << 4) | imm40));
                        std.log.debug("C.LI x{}, {} = ADDI x{}, x0, {}", .{rd, imm, rd, imm});
                        self.seti_reg(rd, imm);
                    } else if (funct == 0b011) {
                        if (rd == 2) { // C.ADDI16SP
                            const imm9 = (instr >> 12) & 0b1;
                            const imm5 = (instr >> 2) & 0b1;
                            const imm87 = (instr >> 3) & 0b11;
                            const imm6 = (instr >> 5) & 0b1;
                            const imm4 = (instr >> 6) & 0b1;
                            const imm = @bitCast(i10, @intCast(u10, (imm9 << 9) | (imm87 << 7) | (imm6 << 6) | (imm5 << 5) | (imm4 << 4)));
                            std.log.debug("C.ADDI16SP {} = ADDI x2, x2, {}", .{imm, imm});
                            self.seti_reg(2, self.geti_reg(2) + imm);
                        } else { // C.LUI
                            const imm40 = (instr >> 2) & 0b11111;
                            const imm5 = (instr >> 11) & 0b1;
                            const imm = ((imm5 << 4) | imm40) << 12;
                            std.log.debug("C.LUI x{}, {}", .{rd, imm});
                            self.set_reg(rd, imm);
                        }
                    } else if (funct == 0b100) {
                        const funct2 = (instr >> 10) & 0b11;
                        const imm5 = (instr >> 12) & 0b1;
                        const imm40 = (instr >> 2) & 0b11111;
                        const imm = @intCast(u6, (imm5 << 5) | imm40);
                        const rD = @intCast(u5, (instr >> 7) & 0b111) + 8;
                        if (funct2 == 0b00 and imm != 0) { // C.SRLI
                            std.log.debug("C.SRLI x{}, 0x{x} = SRLI x{}, x{}, {}", .{rD, imm, rD, rD, imm});
                            self.set_reg(rD, self.get_reg(rD) >> @intCast(u5, imm));
                        } else if (funct2 == 0b10) { // C.ANDI
                            const val = @bitCast(u32, @intCast(i32, @bitCast(i6, imm)));
                            std.log.debug("C.ANDI x{}, 0x{x}", .{rD, val});
                            self.set_reg(rD, self.get_reg(rD) & val);
                        } else if (funct2 == 0b11) {
                            const functs = (instr >> 5) & 0b11;
                            const rs2 = @intCast(u5, (instr >> 2) & 0b111) + 8;
                            if (imm5 == 0 and functs == 0b00) { // C.SUB
                                std.log.debug("C.SUB x{}, x{} = SUB x{}, x{}, x{}", .{rD, rs2, rD, rD, rs2});
                                self.set_reg(rD, self.get_reg(rD) -% self.get_reg(rs2));
                            } else if (functs == 0b01) {
                                if (imm5 == 1) {
                                    std.log.debug("C.ADDW TODO", .{});
                                    self.invalidInstruction();
                                } else {
                                    std.log.debug("C.XOR x{}, x{} = XOR x{}, x{}, x{}", .{rD, rs2, rD, rD, rs2});
                                    self.set_reg(rD, self.get_reg(rD) ^ self.get_reg(rs2));
                                }
                            } else if (functs == 0b10) { // C.OR
                                std.log.debug("C.OR x{}, x{} = OR x{}, x{}, x{}", .{rD, rs2, rD, rD, rs2});
                                self.set_reg(rD, self.get_reg(rD) | self.get_reg(rs2));
                            } else if (functs == 0b11) { // C.AND
                                std.log.debug("C.AND x{}, x{} = AND x{}, x{}, x{}", .{rD, rs2, rD, rD, rs2});
                                self.set_reg(rD, self.get_reg(rD) & self.get_reg(rs2));
                            } else {
                                std.log.err("Unknown compressed opcode, op = 01, funct = {b}, functs = {b}", .{funct, functs});
                                self.invalidInstruction();
                            }
                        } else {
                            std.log.err("Unknown compressed opcode, op = 01, funct = {b}, funct2 = {b}", .{funct, funct2});
                            self.invalidInstruction();
                        }
                    } else if (funct == 0b101) { // C.J
                        const imm11 = (instr >> 12) & 0b1;
                        const imm4 = (instr >> 11) & 0b1;
                        const imm98 = (instr >> 9) & 0b11;
                        const imm10 = (instr >> 8) & 0b1;
                        const imm6 = (instr >> 7) & 0b1;
                        const imm7 = (instr >> 6) & 0b1;
                        const imm31 = (instr >> 3) & 0b111;
                        const imm5 = (instr >> 2) & 0b1;

                        const imm = @bitCast(i12, @intCast(u12, (imm11 << 11) | (imm10 << 10) | (imm98 << 8) |
                            (imm7 << 7) | (imm6 << 6) | (imm5 << 5) | (imm4 << 4) | (imm31 << 1)));
                        const newPc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, imm));
                        std.log.debug("C.J 0x{x} = JAL x0, 0x{x}", .{newPc, newPc});
                        self.pc = newPc;
                        jumped = true;
                    } else if (funct == 0b111) { // C.BNEZ
                        const rs1 = (rd & 0b111) + 8; // @intCast(u5, (instr >> 7) & 0b111) + 8;
                        const imm5 =  (instr >> 2) & 0b1;
                        const imm21 = (instr >> 3) & 0b11;
                        const imm76 = (instr >> 5) & 0b11;
                        const imm43 = (instr >> 10) & 0b11;
                        // const imm8 =  (instr >> 12) & 0b1;
                        const imm = @bitCast(i8, @intCast(u8, (imm76 << 6) | (imm5 << 5) | (imm43 << 3) | (imm21 << 1)));
                        const newPc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, imm));
                        std.log.debug("C.BNEZ x{}, 0x{x} = BNE x{}, x0, 0x{x}", .{rs1, newPc, rs1, newPc});
                        if (self.get_reg(rs1) != 0) {
                            jumped = true;
                            self.pc = newPc;
                        }
                    } else if (funct == 0b110) { // C.BEQZ
                        const rs1 = (rd & 0b111) + 8; // @intCast(u5, (instr >> 7) & 0b111) + 8;
                        const imm5 =  (instr >> 2) & 0b1;
                        const imm21 = (instr >> 3) & 0b11;
                        const imm76 = (instr >> 5) & 0b11;
                        const imm43 = (instr >> 10) & 0b11;
                        // const imm8 =  (instr >> 12) & 0b1;
                        const imm = @bitCast(i8, @intCast(u8, (imm76 << 6) | (imm5 << 5) | (imm43 << 3) | (imm21 << 1)));
                        const newPc = @intCast(u32, @intCast(i33, self.pc) + @intCast(i33, imm));
                        std.log.debug("C.BEQZ x{}, 0x{x} = BEQ x{}, x0, 0x{x}", .{rs1, newPc, rs1, newPc});
                        if (self.get_reg(rs1) == 0) {
                            jumped = true;
                            self.pc = newPc;
                        }
                    }else {
                        std.log.err("Unknown compressed opcode, op = 01, funct = {b}", .{funct});
                        self.invalidInstruction();
                    }
                    compressed = true;
                } else if (comptime riscv.featureSetHas(arch.features, .c) and (opcode & 0x3) == 0b00) {
                    const funct = (instr >> 13) & 0b111;
                    if (funct == 0b000) { // C.ADDI4SPN
                        const imm3 = (instr >> 5) & 0b1;
                        const imm2 = (instr >> 6) & 0b1;
                        const imm96 = (instr >> 7) & 0b1111;
                        const imm54 = (instr >> 11) & 0b11;
                        const imm = (imm96 << 6) | (imm54 << 4) | (imm3 << 3) | (imm2 << 2);
                        const rD = @intCast(u5, (instr >> 2) & 0b111) + 8;
                        std.log.debug("C.ADDI4SPN x{}, {} = ADDI x{}, x2, {}", .{rD, imm, rD, imm});
                        self.set_reg(rD, self.get_reg(2) +% imm);
                    } else if (funct == 0b010) { // C.LW
                        const rs1 = @intCast(u5, (instr >> 7) & 0b111) + 8;
                        const rD = @intCast(u5, (instr >> 2) & 0b111) + 8;
                        const imm2 = (instr >> 6) & 0b1;
                        const imm6 = (instr >> 5) & 0b1;
                        const imm53 = (instr >> 10) & 0b111;
                        const imm = (imm6 << 6) | (imm53 << 3) | (imm2 << 2);
                        const addr = self.get_reg(rs1) + imm;
                        std.log.debug("C.LW x{}, {}(x{})", .{rD, imm, rs1});
                        self.set_reg(rD, self.memory.readIntLittle(u32, addr));
                    } else if (funct == 0b110) { // C.SW
                        const rs1 = rd + 8; // @intCast(u5, (instr >> 7) & 0b111) + 8;
                        const rs2 = @intCast(u5, (instr >> 2) & 0b111) + 8;
                        const imm2 = (instr >> 6) & 0b1;
                        const imm6 = (instr >> 5) & 0b1;
                        const imm53 = (instr >> 10) & 0b111;
                        const imm = (imm6 << 6) | (imm53 << 3) | (imm2 << 2);
                        const addr = self.get_reg(rs1) + imm;
                        std.log.debug("C.SW x{}, {}(x{})", .{rs2, imm, rs1});
                        self.memory.writeIntLittle(u32, addr, self.get_reg(rs2));
                    } else {
                        std.log.err("Unknown compressed opcode, op = 00, funct = {b}", .{funct});
                        self.invalidInstruction();
                    }
                    compressed = true;
                } else if (comptime riscv.featureSetHas(arch.features, .c) and (opcode & 0x3) == 0b10) {
                    const funct = (instr >> 13) & 0b111;
                    if (funct == 0b000) { // C.SLLI
                        const imm5 = (instr >> 12) & 0b1;
                        const imm40 = (instr >> 2) & 0b11111;
                        const imm = @intCast(u5, (imm5 << 5) | imm40);
                        const rD = @intCast(u5, (instr >> 7) & 0b111) + 8;
                        std.log.debug("C.SLLI x{}, {} = SLLI x{}, x{}, {}", .{rD, imm, rD, rD, imm});
                        self.set_reg(rD, self.get_reg(rD) << imm);
                    } else if (funct == 0b110) { // C.SWSP
                        const rs2 = @intCast(u5, (instr >> 2) & 0b11111);
                        const imm76 = (instr >> 7) & 0b11;
                        const imm52 = (instr >> 9) & 0b1111;
                        const imm = (imm76 << 6) | (imm52 << 2);
                        std.log.debug("C.SWSP x{}, {} = SW x{}, {}(x2)", .{rs2, imm, rs2, imm});
                        self.memory.writeIntLittle(u32, self.get_reg(2) + imm, self.get_reg(rs2));
                    } else if (funct == 0b010) { // C.LWSP
                        const imm76 = (instr >> 2) & 0b11;
                        const imm5 = (instr >> 12) & 0b1;
                        const imm42 = (instr >> 4) & 0b111;
                        const imm = (imm76 << 6) | (imm5 << 5) | (imm42 << 2);
                        std.log.debug("C.LWSP x{}, {} {b} {b} {b} = LW x{}, {}(x2)", .{rd, imm, imm76, imm5, imm42, rd, imm});
                        self.set_reg(rd, self.memory.readIntLittle(u32, self.get_reg(2) + imm));
                    } else if (funct == 0b100) {
                        const funct4 = (instr >> 12) & 0b1;
                        if (funct4 == 0) { // C.MV
                            const rs2 = @intCast(u5, (instr >> 2) & 0b11111);
                            if (rs2 == 0) {
                                const rs1 = rd; // = @intCast(u5, (instr >> 7) & 0b11111)
                                if (rs1 != 0) { // C.JR
                                    std.log.debug("C.JR x{} = JALR x0, 0(x{})", .{rs1, rs1});
                                    self.pc = self.get_reg(rs1);
                                    jumped = true;
                                } else {
                                    std.log.err("Invalid compressed opcode, op = 10, funct = {b}", .{funct});
                                    self.invalidInstruction();
                                }
                            } else { // C.MV
                                std.log.debug("C.MV x{}, x{} = ADD x{}, x0, x{}", .{rd, rs2, rd, rs2});
                                self.set_reg(rd, self.get_reg(rs2));
                            }
                        } else {
                            const rs2 = @intCast(u5, (instr >> 2) & 0b11111);
                            if (rs2 == 0) {
                                const rs1 = rd; // = @intCast(u5, (instr >> 7) & 0b11111)
                                if (rs1 == 0) {
                                    std.log.err("TODO: C.EBREAK", .{});
                                    @breakpoint();
                                    //std.process.exit(0);
                                } else {
                                    std.log.err("TODO: C.JALR", .{});
                                }
                            } else { // C.ADD
                                std.log.debug("C.ADD x{}, x{} = ADD x{}, x{}, x{}", .{rd, rs2, rd, rd, rs2});
                                self.set_reg(rd, self.get_reg(rd) +% self.get_reg(rs2));
                            }
                        }
                    } else {
                        std.log.err("Unknown compressed opcode, op = 10, funct = {b}", .{funct});
                        self.invalidInstruction();
                    }
                    compressed = true;
                } else {
                    self.invalidInstruction();
                }
            }
        }

        self.pc += if (jumped) @as(arch.XLEN, 0) else if (compressed) @as(arch.XLEN, 2) else @as(arch.XLEN, 4);
    }
};