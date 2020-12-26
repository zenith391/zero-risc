const STDOUT = @intToPtr(*volatile u8, 0xB8000);
usingnamespace @import("vga.zig");

fn writeChar(char: u8) void {
    STDOUT.* = char;
}

fn print(msg: []const u8) void {
    for (msg) |value| {
        writeChar(value);
    }
}

fn println(msg: []const u8) void {
    print(msg);
    writeChar('\n');
}

export fn _start() noreturn {
    //var i: u8 = 0;
    //while (i < 26) : (i += 1) {
    //    writeChar(65 + i);
    //    writeChar(97 + i);
    //}
    //writeChar('\n');
    var vga = VGA.init();
    vga.println("Test");
    vga.println("RISC-V Testing Operating System");
    vga.println("Version: 0.1\n");
    vga.println("Hello, World !");

    while (true) {}
}
