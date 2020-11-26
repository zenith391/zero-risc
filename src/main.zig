const std = @import("std");
const cpu = @import("cpu.zig");

pub const log_level = .notice;

fn readSlice(userdata: usize, addr: usize) u8 {
    const slice = @intToPtr(*[]u8, userdata);
    return slice.*[addr];
}

fn writeSlice(userdata: usize, addr: usize, value: u8) void {
    if (addr == 0x100) {
        var str = [1]u8 {value};
        std.debug.warn("{}", .{str});
    } else {
        var slice = @intToPtr(*[]u8, userdata);
        slice.*[addr] = value;
    }
}

pub fn defaultMemory(ram: *[]u8) cpu.Memory {
    return .{
        .readFn = readSlice,
        .writeFn = writeSlice,
        .userData = @ptrToInt(ram)
    };
}

fn usage() !void {
    var stderr = std.io.getStdErr().writer();
    try stderr.writeAll("Usage: zerorisc {help | [file]}\n");
    std.process.exit(1);
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        try usage();
    }

    const filePath = args[1];
    if (std.mem.eql(u8, filePath, "help")) {
        var stdout = std.io.getStdOut().writer();
        try stdout.writeAll(
            \\Zero RISC
            \\
            \\IO ports:
            \\  0x100: stdout
            \\
            \\Usage:
            \\  zerorisc { help | [file] }
            \\  Example: zerorisc tests/hello
            \\
        );
        return;
    }

    var file = std.fs.cwd().openFile(filePath, .{ .read = true }) catch |err| {
        std.debug.warn("Could not open '{}', got {} error.\n", .{filePath, @errorName(err)});
        std.process.exit(1);
    };
    defer file.close();

    // Allocate memory
    var ram = try allocator.alignedAlloc(u8, 16, 80 * 1024); // big alignment for better performance
    defer allocator.free(ram);
    var memory = defaultMemory(&ram);

    // Read the ELF file
    var header = try std.elf.readHeader(file);
    const entry = header.entry;
    var pIterator = header.program_header_iterator(file);
    while (try pIterator.next()) |phdr| {
        if (phdr.p_type == std.elf.PT_LOAD) {
            try file.seekTo(phdr.p_offset);
            var data = try allocator.alloc(u8, phdr.p_filesz);
            _ = try file.readAll(data);
            for (data) |val, i| {
                ram[phdr.p_vaddr + i] = val;
            }
            allocator.free(data);
        }
    }

    // Initialise the RISC-V hart
    var hart = cpu.Hart {
        .memory = memory,
        .pc = @intCast(u32, header.entry)
    };

    // Currently it only executes 100 step
    var i: usize = 0;
    while (i < 100) : (i += 1) {
        hart.cycle();
    }

    // Print the first 16 registers (x0-x15) as debug.
    i = 0;
    while (i < 16) : (i += 1) {
        var str = [1]u8 {@intCast(u8, hart.x[i] & 0xFF)};
        std.log.debug("x{} = {x} = {}", .{i, hart.x[i], str});
    }
}
