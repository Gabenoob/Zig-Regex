test {
    const std = @import("std");
    std.testing.refAllDeclsRecursive(@This());
    _ = @import("src/compiler.zig");
    _ = @import("src/vm.zig");
    _ = @import("src/regex.zig");
    _ = @import("src/match.zig");
    _ = @import("src/regex.zig");
    _ = @import("src/regex_tests.zig");
}
