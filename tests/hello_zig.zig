usingnamespace @import("vga.zig");

const GpuMode = enum {
    /// 80x25
    Text,
    Graphics
};

/// GPU operation start at 0xBD800, flush by writing at 0xBD7FF
const GpuOp = union(enum) {
    SetMode: union (GpuMode) {
        Text,
        Graphics: packed struct { width: u32, height: u32 }
    },
    Fill: packed struct {
        x: u16, y: u16,
        w: u16, h: u16,
        rgb: u32
    }
};

fn flushGpu() void {
    @intToPtr(*volatile u8, 0xBD7FF).* = 0xFF;
}

inline fn postCommand(op: GpuOp) void {
    @intToPtr(*volatile GpuOp, 0xBD800).* = op;
    flushGpu();
}

inline fn fill(x: u16, y: u16, w: u16, h: u16, rgb: u32) void {
    postCommand(GpuOp {
        .Fill = .{ .x = x, .y = y, .w = w, .h = h, .rgb = rgb }
    });
}

const Position = packed struct {
    x: u16,
    y: u16
};

const mousePos = @intToPtr(*const volatile Position, 0xD0000);

export fn _start() noreturn {
    // var vga = VGA.init();
    // vga.println("Test");
    // vga.println("RISC-V Testing Operating System");
    // vga.println("Version: 0.1\n");
    // vga.println("Hello, World !");

    postCommand(GpuOp {
        .SetMode = .{ .Graphics = .{ .width = 640, .height = 480 } }
    });

    fill(0, 0, 640, 480, 0xFFFFFF);

    fill(0, 0, 640, 24, 0x444444);
    fill(0, 0, 24, 24, 0xBDBDBD);

    while (true) {
        if (mousePos.y > 24) {
            fill(mousePos.x, mousePos.y, 5, 5, 0x222222);
        }
    }
}
