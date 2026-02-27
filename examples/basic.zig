const std = @import("std");
const regex = @import("re");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("Zig Regex Library Example\n", .{});
    std.debug.print("========================\n\n", .{});

    // Example 1: Simple matching
    std.debug.print("1. Simple matching:\n", .{});
    {
        var re = try regex.Regex.compile(allocator, "hello");
        defer re.deinit();

        var m = try re.match("hello world");
        if (m) |*match_result| {
            std.debug.print("   Pattern 'hello' matches 'hello world': {s}\n", .{
                match_result.fullMatch(),
            });
            match_result.deinit();
        }
    }

    // Example 2: Character classes
    std.debug.print("\n2. Character classes:\n", .{});
    {
        var re = try regex.Regex.compile(allocator, "[0-9]+");
        defer re.deinit();

        var m = try re.search("abc123def");
        if (m) |*match_result| {
            std.debug.print("   Found digits: {s}\n", .{match_result.fullMatch()});
            match_result.deinit();
        }
    }

    // Example 3: Capture groups
    std.debug.print("\n3. Capture groups:\n", .{});
    {
        var re = try regex.Regex.compile(allocator, "(\\w+)=(\\d+)");
        defer re.deinit();

        var m = try re.match("age=25");
        if (m) |*match_result| {
            std.debug.print("   Full match: {s}\n", .{match_result.fullMatch()});
            if (match_result.get(1)) |group1| {
                std.debug.print("   Group 1 (key): {s}\n", .{group1});
            }
            if (match_result.get(2)) |group2| {
                std.debug.print("   Group 2 (value): {s}\n", .{group2});
            }
            match_result.deinit();
        }
    }

    // Example 4: Find all matches
    std.debug.print("\n4. Find all matches:\n", .{});
    {
        var re = try regex.Regex.compile(allocator, "\\w+");
        defer re.deinit();

        const matches = try re.findAll("hello world test");
        defer {
            for (matches) |*m| m.deinit();
            allocator.free(matches);
        }

        std.debug.print("   Words found: ", .{});
        for (matches, 0..) |m, i| {
            if (i > 0) std.debug.print(", ", .{});
            std.debug.print("{s}", .{m.fullMatch()});
        }
        std.debug.print("\n", .{});
    }

    // Example 5: Replace
    std.debug.print("\n5. Replace:\n", .{});
    {
        var re = try regex.Regex.compile(allocator, "world");
        defer re.deinit();

        const result = try re.subAll("universe", "world hello world");
        defer allocator.free(result);

        std.debug.print("   'world hello world' -> '{s}'\n", .{result});
    }

    // Example 6: Split
    std.debug.print("\n6. Split:\n", .{});
    {
        var re = try regex.Regex.compile(allocator, ",");
        defer re.deinit();

        const parts = try re.split("apple,banana,cherry");
        defer {
            for (parts) |p| allocator.free(p);
            allocator.free(parts);
        }

        std.debug.print("   Split result: ", .{});
        for (parts, 0..) |p, i| {
            if (i > 0) std.debug.print(" | ", .{});
            std.debug.print("{s}", .{p});
        }
        std.debug.print("\n", .{});
    }

    // Example 7: Escape special characters
    std.debug.print("\n7. Escape special characters:\n", .{});
    {
        const escaped = try regex.escape(allocator, "a.b*c+d?");
        defer allocator.free(escaped);
        std.debug.print("   Original: a.b*c+d?\n", .{});
        std.debug.print("   Escaped:  {s}\n", .{escaped});
    }

    std.debug.print("\n========================\n", .{});
    std.debug.print("Example completed!\n", .{});
}
