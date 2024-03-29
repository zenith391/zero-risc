const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("zerorisc", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    //exe.addBuildOption([]const u8, "emulated-arch", "RV32IC");
    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("sdl2");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    run_step.dependOn(b.default_step);
}
