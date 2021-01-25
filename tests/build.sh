#!/bin/sh
zig build-exe hello.s -target riscv32-freestanding
zig build-exe hello_zig.zig -target riscv32-freestanding -O Debug
zig build-exe hello_zigg.zig -target riscv64-linux -O ReleaseFast
