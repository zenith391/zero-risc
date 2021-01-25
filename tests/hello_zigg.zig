const std = @import("std");

pub export fn _start() noreturn {
    _ = std.os.write(std.os.STDOUT_FILENO, "Hello, World !\n") catch unreachable;
    _ = std.io.getStdOut().write("Hello, World !\n") catch unreachable;
    @breakpoint();
    while (true) {}
}
