usingnamespace @import("vga.zig");
const std = @import("std");



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

fn postCommand(op: GpuOp) void {
    @intToPtr(*volatile GpuOp, 0xBD800).* = op;
    flushGpu();
}

fn fill(x: u16, y: u16, w: u16, h: u16, rgb: u32) void {
    postCommand(GpuOp {
        .Fill = .{ .x = x, .y = y, .w = w, .h = h, .rgb = rgb }
    });
}

const Position = packed struct {
    x: u16,
    y: u16
};

const mousePos = @intToPtr(*const volatile Position, 0xD0000);

const Container = struct {
    x: u16,
    y: u16,
    background: ?u32,
};

const Button = struct {
    x: u16,
    y: u16,
    width: u16,
    height: u16,
    hovered: bool = false,

    pub fn mouseMoved(self: *Button, x: u16, y: u16) void {
        const hovered = (x >= self.x and x < self.x+self.width) and 
            (y >= self.y and y < self.y+self.height);
        if (self.hovered == hovered) {
            return;
        } else {
            self.hovered = hovered;
            self.draw();
        }
    }

    pub fn draw(self: *Button) void {
        const x = self.x;     const y = self.y;
        const w = self.width; const h = self.height;

        const color: u32 = if (self.hovered) 0x9B9B9B else 0xBDBDBD;
        fill(x, y, w, h, color);
    }
};

var btn = Button { .x = 0, .y = 0, .width = 24, .height = 24 };

fn mouseMotion(x: u16, y: u16) void {
    btn.mouseMoved(x, y);
    if (mousePos.y >= 24) {
        fill(x, y, 5, 5, 0x222222);
    }
}

export fn _start() noreturn {
    // var vga = VGA.init();
    // vga.println("Test");
    // vga.println("RISC-V Testing Operating System");
    // vga.println("Version: 0.1\n");
    // vga.println("Hello, World !");

    postCommand(.{
        .SetMode = .{ .Graphics = .{ .width = 640, .height = 480 } }
    });

    fill(0, 0, 640, 480, 0xFFFFFF);
    fill(0, 0, 640, 24, 0x444444);
    btn.draw();

    while (true) {
        mouseMotion(mousePos.x, mousePos.y);
    }
}
