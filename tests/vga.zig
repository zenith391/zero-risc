pub const VGA = struct {
    vram: *volatile [0x4000]u8,
    cursor: usize = 0,

    pub fn init() VGA {
        return VGA {
            .vram = @intToPtr(*volatile [0x4000]u8, 0xB8000)
        };
    }

    pub fn writeChar(self: *VGA, char: u8) void {
        if (char == '\n') {
            self.cursor += (80 - self.cursor % 80);
            return;
        }
        self.vram[self.cursor] = char;
        self.cursor += 1;
    }

    pub fn print(self: *VGA, msg: []const u8) void {
        for (msg) |value| {
            self.writeChar(value);
        }
    }

    pub fn println(self: *VGA, msg: []const u8) void {
        self.print(msg);
        self.writeChar('\n');
    }
};
