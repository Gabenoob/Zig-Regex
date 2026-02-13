const std = @import("std");
const compiler = @import("compiler.zig");

// Re-export types from compiler for compatibility
pub const Instruction = compiler.Instruction;
pub const CompiledRegex = compiler.CompiledRegex;
pub const CharacterClassBitmap = compiler.CharacterClassBitmap;

/// Represents a capture group
pub const GroupCapture = struct {
    start: ?usize,
    end: ?usize,
};

/// Result of a successful match
pub const MatchResult = struct {
    start: usize,
    end: usize,
    groups: []?GroupCapture,

    pub fn deinit(self: MatchResult, allocator: std.mem.Allocator) void {
        allocator.free(self.groups);
    }
};

/// Thread state for NFA simulation
/// Each thread represents one path through the NFA
pub const Thread = struct {
    pc: u32, // Program counter
    sp: u32, // String position
    saved: []?usize, // Capture group positions [start0, end0, start1, end1, ...]

    /// Create a new thread with allocated saved array
    pub fn init(allocator: std.mem.Allocator, capture_groups: u32) !Thread {
        const saved = try allocator.alloc(?usize, capture_groups * 2);
        for (saved) |*slot| slot.* = null;
        return Thread{
            .pc = 0,
            .sp = 0,
            .saved = saved,
        };
    }

    /// Clone a thread with its own copy of saved positions
    pub fn clone(self: Thread, allocator: std.mem.Allocator) !Thread {
        const saved_copy = try allocator.alloc(?usize, self.saved.len);
        @memcpy(saved_copy, self.saved);
        return Thread{
            .pc = self.pc,
            .sp = self.sp,
            .saved = saved_copy,
        };
    }

    /// Free the thread's saved array
    pub fn deinit(self: Thread, allocator: std.mem.Allocator) void {
        allocator.free(self.saved);
    }
};

/// Thread queue for scheduling
const ThreadQueue = struct {
    threads: std.ArrayList(Thread),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) ThreadQueue {
        return ThreadQueue{
            .threads = .empty,
            .allocator = allocator,
        };
    }

    fn deinit(self: *ThreadQueue) void {
        for (self.threads.items) |thread| {
            thread.deinit(self.allocator);
        }
        self.threads.deinit(self.allocator);
    }

    fn push(self: *ThreadQueue, thread: Thread) !void {
        try self.threads.append(self.allocator, thread);
    }

    fn pop(self: *ThreadQueue) ?Thread {
        if (self.threads.items.len == 0) return null;
        return self.threads.orderedRemove(0);
    }

    fn isEmpty(self: ThreadQueue) bool {
        return self.threads.items.len == 0;
    }
};

/// Tracks visited (pc, sp) pairs to avoid infinite loops
const VisitedSet = struct {
    entries: std.AutoHashMap(struct { u32, u32 }, void),

    fn init(allocator: std.mem.Allocator) VisitedSet {
        return VisitedSet{
            .entries = std.AutoHashMap(struct { u32, u32 }, void).init(allocator),
        };
    }

    fn deinit(self: *VisitedSet) void {
        self.entries.deinit();
    }

    fn contains(self: VisitedSet, pc: u32, sp: u32) bool {
        return self.entries.contains(.{ pc, sp });
    }

    fn add(self: *VisitedSet, pc: u32, sp: u32) !void {
        try self.entries.put(.{ pc, sp }, {});
    }

    fn clear(self: *VisitedSet) void {
        self.entries.clearRetainingCapacity();
    }
};

/// Regex Virtual Machine
/// Implements NFA simulation with backtracking
pub const VM = struct {
    allocator: std.mem.Allocator,
    flags: VMFlags,

    pub const VMFlags = packed struct {
        ignore_case: bool = false,
        multiline: bool = false,
        dotall: bool = false,
    };

    pub fn init(allocator: std.mem.Allocator, flags: VMFlags) VM {
        return VM{
            .allocator = allocator,
            .flags = flags,
        };
    }

    /// Execute the compiled regex against the text starting at start_pos
    /// Returns MatchResult on success, null on failure
    pub fn execute(
        self: *VM,
        compiled: CompiledRegex,
        text: []const u8,
        start_pos: usize,
    ) !?MatchResult {
        var queue = ThreadQueue.init(self.allocator);
        defer queue.deinit();

        var visited = VisitedSet.init(self.allocator);
        defer visited.deinit();

        var best_match: ?MatchResult = null;

        // Initialize with starting thread
        const start_thread = try Thread.init(self.allocator, compiled.capture_groups);
        try queue.push(start_thread);

        while (!queue.isEmpty()) {
            const thread = queue.pop().?;
            defer thread.deinit(self.allocator);

            // Check if we've already visited this (pc, sp) state
            if (visited.contains(thread.pc, thread.sp)) {
                continue;
            }
            try visited.add(thread.pc, thread.sp);

            // Check bounds
            if (thread.pc >= compiled.instructions.len) {
                continue;
            }

            const instruction = compiled.instructions[thread.pc];
            const sp = thread.sp;

            switch (instruction) {
                .Char => |c| {
                    if (sp < text.len and self.matchChar(text[sp], c)) {
                        var new_thread = try thread.clone(self.allocator);
                        new_thread.pc += 1;
                        new_thread.sp += 1;
                        try queue.push(new_thread);
                    }
                },

                .Any => {
                    if (sp < text.len and (self.flags.dotall or text[sp] != '\n')) {
                        var new_thread = try thread.clone(self.allocator);
                        new_thread.pc += 1;
                        new_thread.sp += 1;
                        try queue.push(new_thread);
                    }
                },

                .Range => |range| {
                    if (sp < text.len) {
                        const ch = text[sp];
                        if (ch >= range.start and ch <= range.end) {
                            var new_thread = try thread.clone(self.allocator);
                            new_thread.pc += 1;
                            new_thread.sp += 1;
                            try queue.push(new_thread);
                        }
                    }
                },

                .Class => |bitmap| {
                    if (sp < text.len) {
                        const ch = text[sp];
                        if (bitmap.contains(ch)) {
                            var new_thread = try thread.clone(self.allocator);
                            new_thread.pc += 1;
                            new_thread.sp += 1;
                            try queue.push(new_thread);
                        }
                    }
                },

                .Split => |split| {
                    // For greedy matching: try x first (it will be the longer path)
                    // For non-greedy: order would be reversed during compilation
                    var thread1 = try thread.clone(self.allocator);
                    thread1.pc = split.x;
                    try queue.push(thread1);

                    var thread2 = try thread.clone(self.allocator);
                    thread2.pc = split.y;
                    try queue.push(thread2);
                },

                .Jump => |target| {
                    var new_thread = try thread.clone(self.allocator);
                    new_thread.pc = target;
                    try queue.push(new_thread);
                },

                .Save => |group| {
                    if (group < compiled.capture_groups * 2) {
                        var new_thread = try thread.clone(self.allocator);
                        new_thread.saved[group] = sp;
                        new_thread.pc += 1;
                        try queue.push(new_thread);
                    }
                },

                .Match => {
                    // Found a match!
                    // Adjust positions relative to the full original text
                    const match_start = start_pos;
                    const match_end = sp + start_pos;

                    // Extract capture groups with adjusted positions
                    const groups = try self.allocator.alloc(?GroupCapture, compiled.capture_groups);
                    for (0..compiled.capture_groups) |i| {
                        const start_idx = i * 2;
                        const end_idx = i * 2 + 1;
                        if (start_idx < thread.saved.len and end_idx < thread.saved.len) {
                            const group_start = thread.saved[start_idx];
                            const group_end = thread.saved[end_idx];
                            groups[i] = GroupCapture{
                                .start = if (group_start) |s| s + start_pos else null,
                                .end = if (group_end) |e| e + start_pos else null,
                            };
                        } else {
                            groups[i] = null;
                        }
                    }

                    const result = MatchResult{
                        .start = match_start,
                        .end = match_end,
                        .groups = groups,
                    };

                    // For regex, we want the leftmost-longest match
                    // Check if this is better than current best
                    if (best_match == null or self.isBetterMatch(&result, &best_match.?)) {
                        if (best_match) |bm| {
                            bm.deinit(self.allocator);
                        }
                        best_match = result;
                    } else {
                        result.deinit(self.allocator);
                    }
                },

                .LineStart => {
                    if (self.matchLineStart(text, sp)) {
                        var new_thread = try thread.clone(self.allocator);
                        new_thread.pc += 1;
                        try queue.push(new_thread);
                    }
                },

                .LineEnd => {
                    if (self.matchLineEnd(text, sp)) {
                        var new_thread = try thread.clone(self.allocator);
                        new_thread.pc += 1;
                        try queue.push(new_thread);
                    }
                },

                .WordBoundary => {
                    if (self.matchWordBoundary(text, sp)) {
                        var new_thread = try thread.clone(self.allocator);
                        new_thread.pc += 1;
                        try queue.push(new_thread);
                    }
                },

                .Backref => |group| {
                    // Match previously captured group
                    const start_idx = group * 2;
                    const end_idx = group * 2 + 1;

                    if (start_idx < thread.saved.len and end_idx < thread.saved.len) {
                        const group_start = thread.saved[start_idx];
                        const group_end = thread.saved[end_idx];

                        if (group_start != null and group_end != null) {
                            const captured_text = text[group_start.?..group_end.?];
                            const remaining_text = if (sp < text.len) text[sp..] else "";

                            if (remaining_text.len >= captured_text.len and
                                std.mem.eql(u8, remaining_text[0..captured_text.len], captured_text))
                            {
                                var new_thread = try thread.clone(self.allocator);
                                new_thread.pc += 1;
                                new_thread.sp += @as(u32, @intCast(captured_text.len));
                                try queue.push(new_thread);
                            }
                        }
                    }
                },
            }
        }

        return best_match;
    }

    /// Check if character matches, considering ignore_case flag
    fn matchChar(self: VM, text_char: u8, pattern_char: u8) bool {
        if (self.flags.ignore_case) {
            return std.ascii.toLower(text_char) == std.ascii.toLower(pattern_char);
        }
        return text_char == pattern_char;
    }

    /// Check if position matches start of line/string
    fn matchLineStart(self: VM, text: []const u8, pos: u32) bool {
        if (pos == 0) return true;
        if (self.flags.multiline and pos < text.len and text[pos - 1] == '\n') {
            return true;
        }
        return false;
    }

    /// Check if position matches end of line/string
    fn matchLineEnd(self: VM, text: []const u8, pos: u32) bool {
        if (pos == text.len) return true;
        if (self.flags.multiline and pos < text.len and text[pos] == '\n') {
            return true;
        }
        return false;
    }

    /// Check if position is at a word boundary
    fn matchWordBoundary(_: VM, text: []const u8, pos: u32) bool {
        const is_word_char = struct {
            fn check(c: u8) bool {
                return std.ascii.isAlphanumeric(c) or c == '_';
            }
        }.check;

        const left_is_word = pos > 0 and is_word_char(text[pos - 1]);
        const right_is_word = pos < text.len and is_word_char(text[pos]);

        return left_is_word != right_is_word;
    }

    /// Determine if new_match is better than current best_match
    /// Uses leftmost-longest semantics
    fn isBetterMatch(_: VM, new_match: *const MatchResult, best_match: *const MatchResult) bool {
        // Prefer matches that start earlier
        if (new_match.start < best_match.start) return true;
        if (new_match.start > best_match.start) return false;

        // If same start, prefer longer match
        return new_match.end > best_match.end;
    }
};

/// Convenience function to search for a match anywhere in the text
pub fn search(
    allocator: std.mem.Allocator,
    compiled: CompiledRegex,
    text: []const u8,
    flags: VM.VMFlags,
) !?MatchResult {
    var vm = VM.init(allocator, flags);

    // Try starting from each position
    var pos: usize = 0;
    while (pos <= text.len) : (pos += 1) {
        if (try vm.execute(compiled, text[pos..], pos)) |match| {
            return match;
        }
    }

    return null;
}

/// Convenience function to find all non-overlapping matches
pub fn findAll(
    allocator: std.mem.Allocator,
    compiled: CompiledRegex,
    text: []const u8,
    flags: VM.VMFlags,
) ![]MatchResult {
    var results: std.ArrayList(MatchResult) = .empty;
    errdefer {
        for (results.items) |match| {
            match.deinit(allocator);
        }
        results.deinit(allocator);
    }

    var vm = VM.init(allocator, flags);
    var pos: usize = 0;

    while (pos <= text.len) {
        if (try vm.execute(compiled, text[pos..], pos)) |match| {
            // Match positions are already adjusted by execute()
            try results.append(allocator, match);

            // Move past this match (non-overlapping)
            if (match.end == match.start) {
                // Zero-width match, advance by 1
                pos += 1;
            } else {
                pos = match.end;
            }
        } else {
            pos += 1;
        }
    }

    return results.toOwnedSlice(allocator);
}

/// Test utilities and examples
const testing = std.testing;

test "VM basic char matching" {
    const allocator = testing.allocator;

    // Simple pattern: "ab"
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    const instructions = try arena_allocator.alloc(Instruction, 3);
    instructions[0] = .{ .Char = 'a' };
    instructions[1] = .{ .Char = 'b' };
    instructions[2] = .Match;

    const compiled = CompiledRegex{
        .instructions = instructions,
        .capture_groups = 1,
        .allocator = arena_allocator,
    };

    var vm = VM.init(allocator, .{});
    const result = try vm.execute(compiled, "ab", 0);

    try testing.expect(result != null);
    try testing.expectEqual(@as(usize, 0), result.?.start);
    try testing.expectEqual(@as(usize, 2), result.?.end);

    if (result) |m| m.deinit(allocator);
}

test "VM alternation with Split" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Pattern: "a|b" - match 'a' or 'b'
    const instructions = try arena_allocator.alloc(Instruction, 5);
    instructions[0] = .{ .Split = .{ .x = 1, .y = 3 } };
    instructions[1] = .{ .Char = 'a' };
    instructions[2] = .{ .Jump = 4 };
    instructions[3] = .{ .Char = 'b' };
    instructions[4] = .Match;

    const compiled = CompiledRegex{
        .instructions = instructions,
        .capture_groups = 1,
        .allocator = arena_allocator,
    };

    var vm = VM.init(allocator, .{});

    const result1 = try vm.execute(compiled, "a", 0);
    try testing.expect(result1 != null);
    if (result1) |m| m.deinit(allocator);

    const result2 = try vm.execute(compiled, "b", 0);
    try testing.expect(result2 != null);
    if (result2) |m| m.deinit(allocator);
}

test "VM capture groups" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Pattern: "(ab)" - capture group around "ab"
    const instructions = try arena_allocator.alloc(Instruction, 5);
    instructions[0] = .{ .Save = 0 }; // start group 0
    instructions[1] = .{ .Char = 'a' };
    instructions[2] = .{ .Char = 'b' };
    instructions[3] = .{ .Save = 1 }; // end group 0
    instructions[4] = .Match;

    const compiled = CompiledRegex{
        .instructions = instructions,
        .capture_groups = 1,
        .allocator = arena_allocator,
    };

    var vm = VM.init(allocator, .{});
    const result = try vm.execute(compiled, "ab", 0);

    try testing.expect(result != null);
    try testing.expectEqual(@as(usize, 0), result.?.groups[0].?.start);
    try testing.expectEqual(@as(usize, 2), result.?.groups[0].?.end);

    if (result) |m| m.deinit(allocator);
}

test "VM word boundary" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Pattern: \bword\b
    const instructions = try arena_allocator.alloc(Instruction, 7);
    instructions[0] = .WordBoundary;
    instructions[1] = .{ .Char = 'w' };
    instructions[2] = .{ .Char = 'o' };
    instructions[3] = .{ .Char = 'r' };
    instructions[4] = .{ .Char = 'd' };
    instructions[5] = .WordBoundary;
    instructions[6] = .Match;

    const compiled = CompiledRegex{
        .instructions = instructions,
        .capture_groups = 1,
        .allocator = arena_allocator,
    };

    var vm = VM.init(allocator, .{});

    // Should match at word boundaries
    const result1 = try vm.execute(compiled, "word", 0);
    try testing.expect(result1 != null);
    if (result1) |m| m.deinit(allocator);

    // Should not match when not at boundary
    const result2 = try vm.execute(compiled, "sword", 0);
    try testing.expect(result2 == null);
}

test "VM backreference" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Pattern: (a)\1 - capture 'a' then match same again
    const instructions = try arena_allocator.alloc(Instruction, 5);
    instructions[0] = .{ .Save = 0 }; // start group 0
    instructions[1] = .{ .Char = 'a' };
    instructions[2] = .{ .Save = 1 }; // end group 0
    instructions[3] = .{ .Backref = 0 }; // match group 0 again
    instructions[4] = .Match;

    const compiled = CompiledRegex{
        .instructions = instructions,
        .capture_groups = 1,
        .allocator = arena_allocator,
    };

    var vm = VM.init(allocator, .{});

    const result = try vm.execute(compiled, "aa", 0);
    try testing.expect(result != null);
    try testing.expectEqual(@as(usize, 2), result.?.end);

    if (result) |m| m.deinit(allocator);

    // Should not match "ab"
    const result2 = try vm.execute(compiled, "ab", 0);
    try testing.expect(result2 == null);
}

test "VM character class" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Create a bitmap for [abc]
    var bitmap = CharacterClassBitmap.init();
    bitmap.set('a');
    bitmap.set('b');
    bitmap.set('c');

    const instructions = try arena_allocator.alloc(Instruction, 2);
    instructions[0] = .{ .Class = bitmap };
    instructions[1] = .Match;

    const compiled = CompiledRegex{
        .instructions = instructions,
        .capture_groups = 1,
        .allocator = arena_allocator,
    };

    var vm = VM.init(allocator, .{});

    const result1 = try vm.execute(compiled, "a", 0);
    try testing.expect(result1 != null);
    if (result1) |m| m.deinit(allocator);

    const result2 = try vm.execute(compiled, "d", 0);
    try testing.expect(result2 == null);
}

test "VM search function" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    // Pattern: "ab"
    const instructions = try arena_allocator.alloc(Instruction, 3);
    instructions[0] = .{ .Char = 'a' };
    instructions[1] = .{ .Char = 'b' };
    instructions[2] = .Match;

    const compiled = CompiledRegex{
        .instructions = instructions,
        .capture_groups = 1,
        .allocator = arena_allocator,
    };

    const result = try search(allocator, compiled, "xxabxx", .{});
    try testing.expect(result != null);
    try testing.expectEqual(@as(usize, 2), result.?.start);

    if (result) |m| m.deinit(allocator);
}
