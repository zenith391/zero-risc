const std = @import("std");
const cpu = @import("cpu.zig");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});
const Allocator = std.mem.Allocator;

pub const log_level = .notice;

var screen: [0x4000]u8 = undefined;

fn readSlice(userdata: usize, addr: usize) u8 {
    const slice = @intToPtr(*[]u8, userdata);
    //std.debug.warn("read from 0x{x}\n", .{addr});
    return slice.*[addr];
}

fn writeSlice(userdata: usize, addr: usize, value: u8) void {
    //std.debug.warn("store to 0x{x}\n", .{addr});
    if (addr >= 0xB8000 and addr <= 0xB8000+0x4000) {
        screen[addr - 0xB8000] = value;
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
    const allocator = &gpa.allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len < 2) {
        try usage();
    }

    const filePath = args[1];
    if (std.mem.eql(u8, filePath, "help")) {
        try std.io.getStdOut().writer().writeAll(
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
    var ram = try allocator.alignedAlloc(u8, 16, 128 * 1024); // big alignment for better performance
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

    _ = c.SDL_Init(c.SDL_INIT_VIDEO);
    defer c.SDL_Quit();

    const window = c.SDL_CreateWindow("Zero RISC", c.SDL_WINDOWPOS_CENTERED, c.SDL_WINDOWPOS_CENTERED, 8*80, 16*25, 0).?;
    defer c.SDL_DestroyWindow(window);

    const renderer = c.SDL_CreateRenderer(window, -1, 0).?;
    defer c.SDL_DestroyRenderer(renderer);

    const fontFile = try std.fs.cwd().openFile("unifont.hex", .{ .read = true });
    const font = try loadHexFont(allocator, renderer, fontFile);
    fontFile.close();

    // Initialise the RISC-V hart
    var hart = cpu.Hart.init(memory, @intCast(u32, header.entry), 0);
    hart.x[2] = 0x512; // set sp

    var i: usize = 0;
    while (i < 4000) : (i += 1) {
        var event: c.SDL_Event = undefined;
        if (c.SDL_PollEvent(&event) != 0) {
            if (event.type == c.SDL_QUIT) {
                break;
            }
        }
        hart.cycle();

        _ = c.SDL_RenderClear(renderer);
        var x: usize = 0;
        while (x < 80) : (x += 1) {
            var y: usize = 0;
            while (y < 25) : (y += 1) {
                const pos = x+y*80;
                font.drawChar(x*8, y*16, screen[pos]);
            }
        }
        c.SDL_RenderPresent(renderer);
    }

    // Print the first 16 registers (x0-x15) as debug.
    i = 0;
    while (i < 32) : (i += 1) {
        var str = [1]u8 {@intCast(u8, hart.x[i] & 0xFF)};
        std.log.debug("x{} = 0x{x} = {}", .{i, hart.x[i], str});
    }
}

const Font = struct {
    texture: *c.SDL_Texture,
    renderer: *c.SDL_Renderer,
    total: c_int,
    data: []u8,

    fn drawChar(self: *const Font, x: usize, y: usize, codePoint: u16) void {
        const width = std.math.sqrt(self.total) * 8;
        const src = c.SDL_Rect {
            .x = (codePoint*8) % width, .y = ((codePoint * 8) / width) * 16,
            .w = 8, .h = 16
        };

        const dst = c.SDL_Rect {
            .x = @intCast(c_int, x), .y = @intCast(c_int, y),
            .w = 8, .h = 16
        };
        _ = c.SDL_RenderCopy(self.renderer, self.texture, &src, &dst);
    }
};

fn loadHexFont(allocator: *Allocator, renderer: *c.SDL_Renderer, file: std.fs.File) !Font {
    var reader = file.reader();
    var total: c_int = 0;
    var totalBitmap: []u8 = try allocator.alloc(u8, 0);

    while (true) {
        const line = try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize));
        defer allocator.free(line);
        if (line.len == 0) break;
        total += 1;
        var split = std.mem.split(line, ":");
        const codePoint = try std.fmt.parseUnsigned(u16, split.next().?, 16);
        const bitmap = split.next().?;
        var charPixels: [16]u8 = undefined;
        for (charPixels) |*pixel, i| {
            const digits = bitmap[(i*2)..(i*2+2)];
            pixel.* = try std.fmt.parseUnsigned(u8, digits, 16);
        }
        totalBitmap = try allocator.realloc(totalBitmap, totalBitmap.len + 16);
        @memcpy(totalBitmap[totalBitmap.len-16..].ptr, &charPixels, charPixels.len);
    }
    const width: usize = std.math.sqrt(total) * 8;
    const height: usize = std.math.sqrt(total) * 16;
    var pixels: []u8 = try allocator.alloc(u8, width * height * 3);
    var i: usize = 0;
    var ix: usize = 0;
    var iy: usize = 0;
    while (i < total) : (i += 1) {
        var y: usize = 0;
        while (y < 16) : (y += 1) {
            const row = totalBitmap[i*16+y];
            var bitShift: usize = 0;
            while (bitShift < 8) : (bitShift += 1) {
                const bit = ((row << @intCast(u3, bitShift)) & 0x80) >> 7;
                const pos = (ix+bitShift)*3 + (iy+y)*width*3;
                pixels[pos] = bit * 0xFF;
                pixels[pos+1] = bit * 0xFF;
                pixels[pos+2] = bit * 0xFF;
            }
        }
        ix += 8;
        if (ix >= width) {
            ix = 0;
            iy += 16;
        }
    }
    allocator.free(totalBitmap);
    var surface = c.SDL_CreateRGBSurfaceWithFormatFrom(pixels.ptr, @intCast(c_int, width), @intCast(c_int, height), 24,
        @intCast(c_int, width*3), c.SDL_PIXELFORMAT_RGB24).?;
    var texture = c.SDL_CreateTextureFromSurface(renderer, surface).?;
    c.SDL_FreeSurface(surface);
    allocator.free(pixels);
    return Font {
        .texture = texture,
        .renderer = renderer,
        .data = pixels,
        .total = total
    };
}