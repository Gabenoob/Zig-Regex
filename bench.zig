const std = @import("std");
const re = @import("re");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("Regex Performance Benchmark\n", .{});
    std.debug.print("===========================\n\n", .{});

    // Test 1: Simple alternation with backtracking
    {
        std.debug.print("Test 1: Alternation pattern (a|b)*\n", .{});
        var regex = try re.Regex.compile(allocator, "(a|b)*");
        defer regex.deinit();

        // Create input of 100 'a's
        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(allocator);
        for (0..100) |_| try text.append(allocator, 'a');

        var timer = try std.time.Timer.start();
        if (try regex.match(text.items)) |*m| {
            m.deinit();
        }
        const elapsed = timer.read();
        std.debug.print("  Input length: {d}, Time: {d}us\n\n", .{ text.items.len, elapsed / 1000 });
    }

    // Test 2: Multiple quantifiers (common pattern)
    {
        std.debug.print("Test 2: Multiple quantifiers \\w+\\s+\\w+\n", .{});
        var regex = try re.Regex.compile(allocator, "\\w+\\s+\\w+");
        defer regex.deinit();

        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(allocator);
        try text.appendSlice(allocator, "hello    world test    input here    are   words");

        var timer = try std.time.Timer.start();
        const matches = try regex.findAll(text.items);
        defer {
            for (matches) |*m| m.deinit();
            allocator.free(matches);
        }
        const elapsed = timer.read();
        std.debug.print("  Found {d} matches, Time: {d}us\n\n", .{ matches.len, elapsed / 1000 });
    }

    // Test 3: Nested groups with quantifiers
    {
        std.debug.print("Test 3: Nested groups ((a|b)+)+\n", .{});
        var regex = try re.Regex.compile(allocator, "((a|b)+)+");
        defer regex.deinit();

        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(allocator);
        for (0..50) |_| try text.appendSlice(allocator, "ab");

        var timer = try std.time.Timer.start();
        if (try regex.match(text.items)) |*m| {
            m.deinit();
        }
        const elapsed = timer.read();
        std.debug.print("  Input length: {d}, Time: {d}us\n\n", .{ text.items.len, elapsed / 1000 });
    }

    // Test 4: Character class with quantifier
    {
        std.debug.print("Test 4: Character class [a-z]+\n", .{});
        var regex = try re.Regex.compile(allocator, "[a-z]+");
        defer regex.deinit();

        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(allocator);
        for (0..200) |_| try text.append(allocator, 'a' + @as(u8, @intCast(@rem(std.crypto.random.int(u8), 26))));

        var timer = try std.time.Timer.start();
        const matches = try regex.findAll(text.items);
        defer {
            for (matches) |*m| m.deinit();
            allocator.free(matches);
        }
        const elapsed = timer.read();
        std.debug.print("  Input length: {d}, Found {d} matches, Time: {d}us\n\n", .{ text.items.len, matches.len, elapsed / 1000 });
    }

    // Test 5: Search with potential backtracking
    {
        std.debug.print("Test 5: Complex search pattern \\b\\w+@\\w+\\.\\w+\\b\n", .{});
        var regex = try re.Regex.compile(allocator, "\\b\\w+@\\w+\\.\\w+\\b");
        defer regex.deinit();

        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(allocator);
        
        // Create text with emails scattered throughout
        for (0..100) |i| {
            try text.appendSlice(allocator, "contact user");
            try text.append(allocator, @as(u8, @intCast('0' + @rem(i, 10))));
            try text.appendSlice(allocator, "@example.com for more info and ");
        }

        var timer = try std.time.Timer.start();
        const matches = try regex.findAll(text.items);
        defer {
            for (matches) |*m| m.deinit();
            allocator.free(matches);
        }
        const elapsed = timer.read();
        std.debug.print("  Input length: {d}, Found {d} matches, Time: {d}us\n\n", .{ text.items.len, matches.len, elapsed / 1000 });
    }

    // Test 6: Pattern with many capture groups
    {
        std.debug.print("Test 6: Many capture groups (\\w+)(\\s+)(\\w+)(\\s+)(\\w+)\n", .{});
        var regex = try re.Regex.compile(allocator, "(\\w+)(\\s+)(\\w+)(\\s+)(\\w+)");
        defer regex.deinit();

        var text: std.ArrayList(u8) = .empty;
        defer text.deinit(allocator);
        try text.appendSlice(allocator, "one two three four five six seven eight nine ten");

        var timer = try std.time.Timer.start();
        const matches = try regex.findAll(text.items);
        defer {
            for (matches) |*m| m.deinit();
            allocator.free(matches);
        }
        const elapsed = timer.read();
        std.debug.print("  Input length: {d}, Found {d} matches, Time: {d}us\n\n", .{ text.items.len, matches.len, elapsed / 1000 });
    }

    std.debug.print("\nBenchmark completed!\n", .{});
}
