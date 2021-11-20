const std = @import("std");
const cpu = @import("cpu.zig");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});
const Allocator = std.mem.Allocator;

pub const log_level = .info;

var screen: [0x4000]u8 = undefined;
var running: bool = true;
var renderLock = std.Thread.Mutex {};

fn readSlice(userdata: usize, addr: usize) u8 {
    const slice = @intToPtr(*[]u8, userdata);
    //std.debug.warn("read from 0x{x}\n", .{addr});
    return slice.*[addr];
}

const GpuMode = enum {
    /// 80x25
    Text,
    Graphics
};

/// GPU operation start at 0xBD800, flush by writing at 0xBD7FF
const GpuOp = union(enum) {
    SetMode: union(GpuMode) {
        Text,
        Graphics: packed struct { width: u32, height: u32 }
    },
    Fill: packed struct {
        x: u16, y: u16,
        w: u16, h: u16,
        rgb: u32
    }
};

var currentMode = GpuMode.Text;
var gTexture: *c.SDL_Texture = undefined;
fn writeSlice(userdata: usize, addr: usize, value: u8) void {
    var slice = @intToPtr(*[]u8, userdata);
    if (addr == 0xBD7FF) {
        const op = @ptrCast(*GpuOp, slice.*[0xBD800..].ptr);
        switch (op.*) {
            .SetMode => |sm| {
                switch (sm) {
                    .Text => {
                        std.log.info("set text to 80x25", .{});
                        _ = c.SDL_SetRenderTarget(renderer, gTexture);
                        var x: usize = 0;
                        while (x < 80) : (x += 1) {
                            var y: usize = 0;
                            while (y < 25) : (y += 1) {
                                const pos = x+y*80;
                                font.drawChar(x*8, y*16, screen[pos]);
                            }
                        }
                        _ = c.SDL_SetRenderTarget(renderer, null);
                    },
                    .Graphics => |g| {
                        std.log.info("set graphics to {}x{}", .{g.width, g.height});
                        c.SDL_SetWindowSize(window, @intCast(c_int, g.width), @intCast(c_int, g.height));
                        renderLock.lock();
                        defer renderLock.unlock();
                        gTexture = c.SDL_CreateTexture(renderer, c.SDL_PIXELFORMAT_RGB24, c.SDL_TEXTUREACCESS_TARGET,
                            @intCast(c_int, g.width), @intCast(c_int, g.height)) orelse unreachable;
                    }
                }
            },
            .Fill => |cmd| {
                //std.log.debug("fill {}x{} rectangle at {}x{} with color {x}", .{cmd.w, cmd.h, cmd.x, cmd.y, cmd.rgb});

                const rect = c.SDL_Rect {
                    .x = cmd.x, .y = cmd.y,
                    .w = cmd.w, .h = cmd.h
                };

                renderLock.lock();
                defer renderLock.unlock();

                _ = c.SDL_SetRenderTarget(renderer, gTexture);
                _ = c.SDL_SetRenderDrawColor(renderer, @truncate(u8, cmd.rgb >> 16),
                    @truncate(u8, cmd.rgb >> 8), @truncate(u8, cmd.rgb), 0xFF);
                _ = c.SDL_RenderFillRect(renderer, &rect);
                _ = c.SDL_SetRenderTarget(renderer, null);
            }
        }
    } else if (addr >= 0xB8000 and addr < 0xB8000+0x4000) {
        const pos = (addr - 0xB8000 ) / 2;
        if ((addr - 0xB8000) % 2 == 0) {
            screen[pos] = value;
            if (currentMode == .Text) {
                const y = pos / 80;
                const x = pos % 80;

                renderLock.lock();
                defer renderLock.unlock();

                _ = c.SDL_SetRenderTarget(renderer, gTexture);
                font.drawChar(x*8, y*16, screen[pos]);
                _ = c.SDL_SetRenderTarget(renderer, null);
            }
        }
    } else {
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

fn ecall(hart: *cpu.Hart) void {
    const number = hart.x[17];
    switch (number) {
        64 => { // write
            const fd = hart.x[10];
            const ptr = hart.x[11];
            const count = hart.x[12];
            std.log.scoped(.syscall).debug("write({}, ..., {})", .{fd, count});
            if (fd == 1) { // == STDOUT_FILENO
                var buf: [256]u8 = undefined;
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    buf[i] = hart.memory.read(ptr + i);
                }
                const ret = std.io.getStdOut().write(buf[0..count]) catch unreachable; // TODO: handle error
                hart.x[10] = @truncate(u32, ret); // it is impossible to have written more than count which is 32 bits.
            }
        },
        else => std.log.crit("Unknown syscall: {}", .{number})
    }
}

fn usage() !void {
    var stderr = std.io.getStdErr().writer();
    try stderr.writeAll("Usage: zerorisc {help | [file]}\n");
    std.process.exit(1);
}

fn hartMain(hart: *cpu.Hart) !void {
    while (running) {
        hart.cycle();
        //std.time.sleep(20000);
    }
}

var window: *c.SDL_Window = undefined;
var renderer: *c.SDL_Renderer = undefined;
var font: Font = undefined;

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
        std.debug.warn("Could not open '{s}', got {s} error.\n", .{filePath, @errorName(err)});
        std.process.exit(1);
    };
    defer file.close();

    var useSDL: bool = false;
    _ = useSDL;

    // Allocate memory
    var ram = try allocator.alignedAlloc(u8, 16, 64 * 128 * 1024); // big alignment for better performance
    defer allocator.free(ram);
    var memory = defaultMemory(&ram);

    // Read the ELF file
    var header = try std.elf.Header.read(file);
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
    var hart = cpu.Hart.init(memory, @intCast(u32, header.entry), 0);
    hart.ecallFn = ecall;
    hart.x[2] = 0x512; // set sp

    _ = c.SDL_Init(c.SDL_INIT_VIDEO);
    defer c.SDL_Quit();

    window = c.SDL_CreateWindow("Zero RISC", c.SDL_WINDOWPOS_CENTERED, c.SDL_WINDOWPOS_CENTERED, 8*80, 16*25, 0).?;
    defer c.SDL_DestroyWindow(window);

    renderer = c.SDL_CreateRenderer(window, -1, 0).?;
    defer c.SDL_DestroyRenderer(renderer);

    const fontFile = try std.fs.cwd().openFile("unifont.hex", .{ .read = true });
    font = try loadHexFont(allocator, fontFile);
    fontFile.close();

    var thread = try std.Thread.spawn(.{}, hartMain, .{ &hart });

    gTexture = c.SDL_CreateTexture(renderer, c.SDL_PIXELFORMAT_RGB24, c.SDL_TEXTUREACCESS_TARGET,
        640, 400) orelse unreachable;

    while (true) {
        var event: c.SDL_Event = undefined;
        if (c.SDL_PollEvent(&event) != 0) {
            if (event.type == c.SDL_QUIT) {
                break;
            } else if (event.type == c.SDL_MOUSEMOTION) {
                const mouse = event.motion;
                memory.writeIntLittle(u16, 0xD0000, @intCast(u16, mouse.x));
                memory.writeIntLittle(u16, 0xD0002, @intCast(u16, mouse.y));
            }
        }

        renderLock.lock();
        defer renderLock.unlock();
        _ = c.SDL_RenderClear(renderer);
        _ = c.SDL_RenderCopy(renderer, gTexture, null, null);
        c.SDL_RenderPresent(renderer);
    }
    running = false;
    thread.join();

    // Print the registers (x0-x31) as debug.
    var i: usize = 0;
    while (i < 32) : (i += 1) {
        var str = [1]u8 {@intCast(u8, hart.x[i] & 0xFF)};
        std.log.debug("x{} = 0x{x} = {s}", .{i, hart.x[i], str});
    }
}

const Font = struct {
    texture: *c.SDL_Texture,
    renderer: *c.SDL_Renderer,
    total: c_int,
    data: []u8,

    fn drawChar(self: *const Font, x: usize, y: usize, codePoint: u16) void {
        const width = std.math.sqrt(@intCast(u32, self.total)) * 8;
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

fn loadHexFont(allocator: *Allocator, file: std.fs.File) !Font {
    var reader = file.reader();
    var total: c_int = 0;
    var totalBitmap: []u8 = try allocator.alloc(u8, 0);

    while (true) {
        const line = (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', std.math.maxInt(usize))) orelse break;
        defer allocator.free(line);
        if (line.len == 0) break;
        total += 1;
        var split = std.mem.split(u8, line, ":");
        const codePoint = try std.fmt.parseUnsigned(u16, split.next().?, 16);
        const bitmap = split.next().?;
        var charPixels: [16]u8 = undefined;
        for (charPixels) |*pixel, i| {
            const digits = bitmap[(i*2)..(i*2+2)];
            pixel.* = try std.fmt.parseUnsigned(u8, digits, 16);
        }
        totalBitmap = try allocator.realloc(totalBitmap, totalBitmap.len + 16);

        // TODO: use codepoint info instead!! (codepoint is not always linear)
        _ = codePoint;
        @memcpy(totalBitmap[totalBitmap.len-16..].ptr, &charPixels, charPixels.len);
    }
    const width: usize = std.math.sqrt(@intCast(usize, total)) * 8;
    const height: usize = std.math.sqrt(@intCast(usize, total)) * 16;
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