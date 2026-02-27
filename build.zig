const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{ .preferred_optimize_mode = .ReleaseFast });

    // Create library module
    const regex_mod = b.addModule("regex", .{
        .root_source_file = b.path("regex.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Run example
    const exe = b.addExecutable(.{
        .name = "regex-example",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/basic.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "re", .module = regex_mod }},
        }),
    });
    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the example");
    run_step.dependOn(&run_exe.step);

    // Run tests
    const test_step = b.step("test", "Run library tests");

    // Test each module individually (inline tests only)
    const tests = b.addTest(.{ .name = "test", .root_module = b.addModule("test", .{
        .root_source_file = b.path("test.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "re", .module = regex_mod },
        },
    }) });
    // Depend on all tests
    test_step.dependOn(&b.addRunArtifact(tests).step);

    // Benchmark executable
    const bench_exe = b.addExecutable(.{
        .name = "regex-bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{ .name = "re", .module = regex_mod }},
        }),
    });

    const run_bench = b.addRunArtifact(bench_exe);
    const bench_step = b.step("bench", "Run performance benchmark");
    bench_step.dependOn(&run_bench.step);
}
