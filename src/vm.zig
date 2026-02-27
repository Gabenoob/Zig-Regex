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

/// Thread ID for slot-based allocation
const ThreadId = u32;

/// Thread state stored in the pool
const ThreadState = struct {
    pc: u32,
    sp: u32,
    saved_generation: u32, // Generation to identify which saved state belongs to this thread
};

/// Thread pool with arena-style allocation for saved arrays
/// Uses generational indices to avoid copying saved arrays on every operation
pub const ThreadPool = struct {
    states: []ThreadState,
    free_stack: []ThreadId,
    free_count: u32,
    
    // Arena for saved arrays - allocated in chunks
    saved_arena: []?usize,       // Contiguous block of saved array slots
    saved_generation: []u32,     // Generation for each slot
    saved_slots_per_thread: u32,
    next_generation: u32,
    
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, max_threads: u32, capture_groups: u32) !ThreadPool {
        const states = try allocator.alloc(ThreadState, max_threads);
        errdefer allocator.free(states);

        const free_stack = try allocator.alloc(ThreadId, max_threads);
        errdefer allocator.free(free_stack);

        const saved_slots_per_thread = capture_groups * 2;
        const total_saved_slots = @as(usize, max_threads) * saved_slots_per_thread;
        
        const saved_arena = try allocator.alloc(?usize, total_saved_slots);
        errdefer allocator.free(saved_arena);
        @memset(saved_arena, null);
        
        const saved_generation = try allocator.alloc(u32, max_threads);
        errdefer allocator.free(saved_generation);
        @memset(saved_generation, 0);

        // Initialize all slots as free (in reverse order)
        var i: u32 = 0;
        while (i < max_threads) : (i += 1) {
            free_stack[i] = max_threads - i - 1;
        }

        return ThreadPool{
            .states = states,
            .free_stack = free_stack,
            .free_count = max_threads,
            .saved_arena = saved_arena,
            .saved_generation = saved_generation,
            .saved_slots_per_thread = saved_slots_per_thread,
            .next_generation = 1,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ThreadPool) void {
        self.allocator.free(self.states);
        self.allocator.free(self.free_stack);
        self.allocator.free(self.saved_arena);
        self.allocator.free(self.saved_generation);
    }

    /// Get pointer to saved array for a thread
    fn getSavedPtr(self: *ThreadPool, thread_id: ThreadId) []?usize {
        const start = @as(usize, thread_id) * self.saved_slots_per_thread;
        return self.saved_arena[start .. start + self.saved_slots_per_thread];
    }

    /// Allocate a new thread slot
    pub fn allocThread(self: *ThreadPool, pc: u32, sp: u32) !ThreadId {
        if (self.free_count == 0) return error.OutOfMemory;

        self.free_count -= 1;
        const id = self.free_stack[self.free_count];

        self.states[id] = .{
            .pc = pc,
            .sp = sp,
            .saved_generation = self.next_generation,
        };
        
        // Clear saved array for this thread
        const saved = self.getSavedPtr(id);
        @memset(saved, null);
        
        self.saved_generation[id] = self.next_generation;
        self.next_generation += 1;

        return id;
    }

    /// Allocate a thread by cloning another thread's saved state
    pub fn allocThreadClone(self: *ThreadPool, pc: u32, sp: u32, source_thread: ThreadId) !ThreadId {
        if (self.free_count == 0) return error.OutOfMemory;

        self.free_count -= 1;
        const new_id = self.free_stack[self.free_count];
        const source_gen = self.states[source_thread].saved_generation;

        self.states[new_id] = .{
            .pc = pc,
            .sp = sp,
            .saved_generation = source_gen,
        };

        // Copy saved array from source
        const source_saved = self.getSavedPtr(source_thread);
        const dest_saved = self.getSavedPtr(new_id);
        @memcpy(dest_saved, source_saved);

        return new_id;
    }

    /// Free a thread slot back to the pool
    pub fn freeThread(self: *ThreadPool, id: ThreadId) void {
        // Just mark slot as free - no need to clear saved array
        self.free_stack[self.free_count] = id;
        self.free_count += 1;
    }

    /// Get thread state
    pub fn get(self: ThreadPool, id: ThreadId) ThreadState {
        return self.states[id];
    }

    /// Update thread position
    pub fn setPos(self: *ThreadPool, id: ThreadId, pc: u32, sp: u32) void {
        self.states[id].pc = pc;
        self.states[id].sp = sp;
    }

    /// Get saved array for a thread
    pub fn getSaved(self: *ThreadPool, id: ThreadId) []?usize {
        return self.getSavedPtr(id);
    }

    /// Update a saved position - marks thread with new generation
    pub fn setSaved(self: *ThreadPool, id: ThreadId, index: u32, value: ?usize) void {
        const saved = self.getSavedPtr(id);
        saved[index] = value;
    }
};

/// Fast circular queue for thread scheduling using O(1) operations
const ThreadQueue = struct {
    buffer: []ThreadId,
    head: u32,
    tail: u32,
    count: u32,
    capacity: u32,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, capacity: u32) !ThreadQueue {
        const buffer = try allocator.alloc(ThreadId, capacity);
        return ThreadQueue{
            .buffer = buffer,
            .head = 0,
            .tail = 0,
            .count = 0,
            .capacity = capacity,
            .allocator = allocator,
        };
    }

    fn deinit(self: *ThreadQueue) void {
        self.allocator.free(self.buffer);
    }

    fn push(self: *ThreadQueue, thread_id: ThreadId) !void {
        if (self.count >= self.capacity) return error.OutOfMemory;
        self.buffer[self.tail] = thread_id;
        self.tail = (self.tail + 1) % self.capacity;
        self.count += 1;
    }

    fn pop(self: *ThreadQueue) ?ThreadId {
        if (self.count == 0) return null;
        const id = self.buffer[self.head];
        self.head = (self.head + 1) % self.capacity;
        self.count -= 1;
        return id;
    }

    fn isEmpty(self: ThreadQueue) bool {
        return self.count == 0;
    }
};

/// Fast visited set using a flat array with versioning
/// This avoids clearing the entire array between executions
const VisitedSet = struct {
    entries: []u32,      // Version number for each (pc, sp) pair
    current_version: u32,
    sp_stride: u32,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, max_pc: u32, max_sp: u32) !VisitedSet {
        // Allocate flat array: each pc has a block of sp entries
        // Round max_sp up to multiple of 64 for cache alignment
        const sp_stride = (max_sp + 63) & ~@as(u32, 63);
        const total_entries = @as(usize, max_pc) * sp_stride;

        const entries = try allocator.alloc(u32, total_entries);
        @memset(entries, 0);

        return VisitedSet{
            .entries = entries,
            .current_version = 1,
            .sp_stride = sp_stride,
            .allocator = allocator,
        };
    }

    fn deinit(self: *VisitedSet) void {
        self.allocator.free(self.entries);
    }

    fn contains(self: VisitedSet, pc: u32, sp: u32) bool {
        const idx = @as(usize, pc) * self.sp_stride + sp;
        if (idx >= self.entries.len) return false;
        return self.entries[idx] == self.current_version;
    }

    fn add(self: *VisitedSet, pc: u32, sp: u32) void {
        const idx = @as(usize, pc) * self.sp_stride + sp;
        if (idx < self.entries.len) {
            self.entries[idx] = self.current_version;
        }
    }

    fn clear(self: *VisitedSet) void {
        // Instead of clearing the whole array, just increment version
        // When version wraps around, we need to clear
        self.current_version += 1;
        if (self.current_version == 0) {
            @memset(self.entries, 0);
            self.current_version = 1;
        }
    }
};

/// Regex Virtual Machine
/// Implements optimized NFA simulation with backtracking
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
        // Estimate capacity: based on pattern complexity
        // Worst case: alternation creates 2 threads per instruction
        const max_threads = @max(2048, compiled.instructions.len * 4);

        const max_pc = @as(u32, @intCast(compiled.instructions.len));
        const max_sp = @as(u32, @intCast(text.len + 1));

        var pool = try ThreadPool.init(self.allocator, @intCast(max_threads), compiled.capture_groups);
        defer pool.deinit();

        var queue = try ThreadQueue.init(self.allocator, @intCast(max_threads));
        defer queue.deinit();

        var visited = try VisitedSet.init(self.allocator, max_pc, max_sp);
        defer visited.deinit();

        var best_match: ?MatchResult = null;
        errdefer if (best_match) |bm| bm.deinit(self.allocator);

        // Initialize with starting thread
        const start_thread = try pool.allocThread(0, 0);
        try queue.push(start_thread);

        while (!queue.isEmpty()) {
            const thread_id = queue.pop().?;
            const thread = pool.get(thread_id);

            // Check if we've already visited this (pc, sp) state
            if (visited.contains(thread.pc, thread.sp)) {
                pool.freeThread(thread_id);
                continue;
            }
            visited.add(thread.pc, thread.sp);

            // Check bounds
            if (thread.pc >= compiled.instructions.len) {
                pool.freeThread(thread_id);
                continue;
            }

            const instruction = compiled.instructions[thread.pc];
            const sp = thread.sp;

            switch (instruction) {
                .Char => |c| {
                    if (sp < text.len and self.matchChar(text[sp], c)) {
                        const new_thread = try pool.allocThreadClone(
                            thread.pc + 1,
                            sp + 1,
                            thread_id,
                        );
                        try queue.push(new_thread);
                    }
                    pool.freeThread(thread_id);
                },

                .Any => {
                    if (sp < text.len and (self.flags.dotall or text[sp] != '\n')) {
                        const new_thread = try pool.allocThreadClone(
                            thread.pc + 1,
                            sp + 1,
                            thread_id,
                        );
                        try queue.push(new_thread);
                    }
                    pool.freeThread(thread_id);
                },

                .Range => |range| {
                    if (sp < text.len) {
                        const ch = text[sp];
                        if (ch >= range.start and ch <= range.end) {
                            const new_thread = try pool.allocThreadClone(
                                thread.pc + 1,
                                sp + 1,
                                thread_id,
                            );
                            try queue.push(new_thread);
                        }
                    }
                    pool.freeThread(thread_id);
                },

                .Class => |bitmap| {
                    if (sp < text.len) {
                        const ch = text[sp];
                        if (bitmap.contains(ch)) {
                            const new_thread = try pool.allocThreadClone(
                                thread.pc + 1,
                                sp + 1,
                                thread_id,
                            );
                            try queue.push(new_thread);
                        }
                    }
                    pool.freeThread(thread_id);
                },

                .Split => |split| {
                    // For greedy matching: try x first
                    const thread1 = try pool.allocThreadClone(split.x, sp, thread_id);
                    try queue.push(thread1);

                    const thread2 = try pool.allocThreadClone(split.y, sp, thread_id);
                    try queue.push(thread2);
                    
                    // Original thread is "consumed" - free it
                    pool.freeThread(thread_id);
                },

                .Jump => |target| {
                    const new_thread = try pool.allocThreadClone(target, sp, thread_id);
                    try queue.push(new_thread);
                    pool.freeThread(thread_id);
                },

                .Save => |group| {
                    if (group < compiled.capture_groups * 2) {
                        // Create new thread with modified saved state
                        const new_thread = try pool.allocThreadClone(thread.pc + 1, sp, thread_id);
                        pool.setSaved(new_thread, group, sp);
                        try queue.push(new_thread);
                    }
                    pool.freeThread(thread_id);
                },

                .Match => {
                    // Found a match!
                    const match_start = start_pos;
                    const match_end = sp + start_pos;
                    const saved = pool.getSaved(thread_id);

                    // Extract capture groups with adjusted positions
                    const groups = try self.allocator.alloc(?GroupCapture, compiled.capture_groups);
                    for (0..compiled.capture_groups) |i| {
                        const start_idx = i * 2;
                        const end_idx = i * 2 + 1;
                        if (start_idx < saved.len and end_idx < saved.len) {
                            const group_start = saved[start_idx];
                            const group_end = saved[end_idx];
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
                    if (best_match == null or self.isBetterMatch(&result, &best_match.?)) {
                        if (best_match) |bm| {
                            bm.deinit(self.allocator);
                        }
                        best_match = result;
                    } else {
                        result.deinit(self.allocator);
                    }
                    
                    pool.freeThread(thread_id);
                },

                .LineStart => {
                    if (self.matchLineStart(text, sp)) {
                        const new_thread = try pool.allocThreadClone(thread.pc + 1, sp, thread_id);
                        try queue.push(new_thread);
                    }
                    pool.freeThread(thread_id);
                },

                .LineEnd => {
                    if (self.matchLineEnd(text, sp)) {
                        const new_thread = try pool.allocThreadClone(thread.pc + 1, sp, thread_id);
                        try queue.push(new_thread);
                    }
                    pool.freeThread(thread_id);
                },

                .WordBoundary => {
                    if (self.matchWordBoundary(text, sp)) {
                        const new_thread = try pool.allocThreadClone(thread.pc + 1, sp, thread_id);
                        try queue.push(new_thread);
                    }
                    pool.freeThread(thread_id);
                },

                .Backref => |group| {
                    const saved = pool.getSaved(thread_id);
                    const start_idx = group * 2;
                    const end_idx = group * 2 + 1;

                    if (start_idx < saved.len and end_idx < saved.len) {
                        const group_start = saved[start_idx];
                        const group_end = saved[end_idx];

                        if (group_start != null and group_end != null) {
                            const captured_text = text[group_start.?..group_end.?];
                            const remaining_text = if (sp < text.len) text[sp..] else "";

                            if (remaining_text.len >= captured_text.len and
                                std.mem.eql(u8, remaining_text[0..captured_text.len], captured_text))
                            {
                                const new_thread = try pool.allocThreadClone(
                                    thread.pc + 1,
                                    sp + @as(u32, @intCast(captured_text.len)),
                                    thread_id,
                                );
                                try queue.push(new_thread);
                            }
                        }
                    }
                    pool.freeThread(thread_id);
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

/// Extract a literal prefix from the compiled regex
/// Returns the number of consecutive Char instructions at the start (after Save)
fn getLiteralPrefixLen(compiled: CompiledRegex) usize {
    // Skip Save(0) at the beginning (entry point for group 0)
    var i: usize = 0;
    if (i < compiled.instructions.len and compiled.instructions[i] == .Save) {
        i += 1;
    }
    
    // Count consecutive Char instructions
    var len: usize = 0;
    while (i + len < compiled.instructions.len) : (len += 1) {
        switch (compiled.instructions[i + len]) {
            .Char => {},
            else => break,
        }
    }
    
    return len;
}

/// Extract a literal suffix from the compiled regex (before Match)
/// Returns (start_index, length) of consecutive Char instructions at the end
fn getLiteralSuffix(compiled: CompiledRegex) struct { start: usize, len: usize } {
    if (compiled.instructions.len < 2) return .{ .start = 0, .len = 0 };
    
    // Work backwards from Match instruction
    var i = compiled.instructions.len - 1;
    
    // Skip Match instruction
    if (compiled.instructions[i] == .Match) {
        if (i == 0) return .{ .start = 0, .len = 0 };
        i -= 1;
    }
    
    // Skip Save(1) - end of group 0
    if (compiled.instructions[i] == .Save) {
        if (i == 0) return .{ .start = 0, .len = 0 };
        i -= 1;
    }
    
    // Count consecutive Char instructions backwards
    const end = i;
    var char_count: usize = 0;
    while (true) {
        switch (compiled.instructions[i]) {
            .Char => {
                char_count += 1;
                if (i == 0) break;
                i -= 1;
            },
            else => break,
        }
    }
    
    if (char_count == 0) return .{ .start = 0, .len = 0 };
    if (char_count > end + 1) return .{ .start = 0, .len = 0 }; // Safety check
    
    const start = end - (char_count - 1);
    return .{ .start = start, .len = char_count };
}

/// Check if pattern has a greedy .* or .+ after the prefix and before the suffix
/// Handles both (.*) and (.*)? patterns
fn hasGreedyDotStarBetween(compiled: CompiledRegex, prefix_end: usize, suffix_start: usize) bool {
    // Look for pattern: Save(group), Split(body, end), body: Any/Class + Jump, end: Save(group+1)
    // Or for optional: Split(skip, group_body), Save(group), Split(body, end), Any, Jump, Save(group+1)
    if (suffix_start <= prefix_end) return false;
    
    var i = prefix_end;
    
    // For optional patterns like (.*)?, the sequence starts with Split (optional branch)
    // Check: Split followed by Save indicates optional group
    if (i < compiled.instructions.len and compiled.instructions[i] == .Split) {
        if (i + 1 < compiled.instructions.len and compiled.instructions[i + 1] == .Save) {
            // This is (.*)? - skip the leading Split and the Save
            i += 2; // Skip Split and Save
        }
        // Otherwise it might be a non-capturing group or other structure - fall through
    }
    
    // Skip any Save instructions (capture groups) for non-optional patterns
    while (i < compiled.instructions.len and compiled.instructions[i] == .Save) : (i += 1) {}
    
    if (i >= suffix_start) return false;
    
    // Check for Split instruction (the loop)
    if (compiled.instructions[i] != .Split) return false;
    i += 1;
    
    // Check for Any or Class (the . in .*)
    if (i >= suffix_start) return false;
    switch (compiled.instructions[i]) {
        .Any, .Class => {},
        else => return false,
    }
    i += 1;
    
    // Check for Jump back (completing the loop)
    if (i >= suffix_start) return false;
    if (compiled.instructions[i] != .Jump) return false;
    i += 1;
    
    // We should be at Save(group+1) now
    if (i < suffix_start and compiled.instructions[i] == .Save) {
        i += 1;
    }
    
    // Now we should be at the suffix (Char instructions)
    return i <= suffix_start;
}

/// Convenience function to search for a match anywhere in the text
/// Uses literal prefix and suffix optimizations when possible
pub fn search(
    allocator: std.mem.Allocator,
    compiled: CompiledRegex,
    text: []const u8,
    flags: VM.VMFlags,
) !?MatchResult {
    var vm = VM.init(allocator, flags);

    // Try to extract literal prefix and suffix
    const prefix_len = getLiteralPrefixLen(compiled);
    const suffix = getLiteralSuffix(compiled);
    
    // Check if we have the pattern: prefix + greedy .* + suffix
    // This is a very common pattern that can be optimized
    const suffix_optimizable = prefix_len >= 3 and suffix.len >= 3;
    const might_have_dot_star = suffix_optimizable and 
        hasGreedyDotStarBetween(compiled, prefix_len + 1, suffix.start);
    

    
    if (might_have_dot_star) {
        // Build prefix string
        var prefix_start: usize = 0;
        if (prefix_start < compiled.instructions.len and compiled.instructions[prefix_start] == .Save) {
            prefix_start += 1;
        }
        
        var prefix_buf: [256]u8 = undefined;
        for (0..prefix_len) |i| {
            prefix_buf[i] = compiled.instructions[prefix_start + i].Char;
        }
        const prefix = prefix_buf[0..prefix_len];
        
        // Build suffix string
        var suffix_buf: [256]u8 = undefined;
        for (0..suffix.len) |i| {
            suffix_buf[i] = compiled.instructions[suffix.start + i].Char;
        }
        const suffix_str = suffix_buf[0..suffix.len];
        
        // Find prefix position
        var search_pos: usize = 0;
        while (search_pos < text.len) {
            if (std.mem.indexOfPos(u8, text, search_pos, prefix)) |prefix_pos| {
                // Look for suffix from the end of the string (greedy match)
                const after_prefix = prefix_pos + prefix_len;
                if (after_prefix > text.len) break;
                
                // Search for suffix from the end (greedy behavior)
                if (std.mem.lastIndexOf(u8, text[after_prefix..], suffix_str)) |suffix_rel_pos| {
                    const suffix_pos = after_prefix + suffix_rel_pos;
                    
                    // Construct the match result
                    // Group 0 is the full match from prefix_pos to suffix_pos + suffix.len
                    const match_end = suffix_pos + suffix.len;
                    
                    const groups = try allocator.alloc(?GroupCapture, compiled.capture_groups);
                    @memset(groups, null);
                    
                    // Group 0: full match
                    groups[0] = .{ .start = prefix_pos, .end = match_end };
                    
                    // Group 1 (the capture in (.*)): from after_prefix to suffix_pos
                    if (compiled.capture_groups > 1) {
                        groups[1] = .{ .start = after_prefix, .end = suffix_pos };
                    }
                    
                    return MatchResult{
                        .start = prefix_pos,
                        .end = match_end,
                        .groups = groups,
                    };
                }
                
                // No suffix found after this prefix, try next prefix occurrence
                search_pos = prefix_pos + 1;
            } else {
                break;
            }
        }
        return null;
    }
    
    // Fall back to prefix-only optimization
    if (prefix_len >= 3) {
        var start_idx: usize = 0;
        if (start_idx < compiled.instructions.len and compiled.instructions[start_idx] == .Save) {
            start_idx += 1;
        }
        
        var prefix_buf: [256]u8 = undefined;
        if (prefix_len <= prefix_buf.len) {
            for (0..prefix_len) |i| {
                prefix_buf[i] = compiled.instructions[start_idx + i].Char;
            }
            const prefix = prefix_buf[0..prefix_len];
            
            var search_pos: usize = 0;
            while (search_pos < text.len) {
                if (std.mem.indexOfPos(u8, text, search_pos, prefix)) |match_pos| {
                    if (try vm.execute(compiled, text[match_pos..], match_pos)) |match| {
                        return match;
                    }
                    search_pos = match_pos + 1;
                } else {
                    break;
                }
            }
            return null;
        }
    }

    // Fall back to brute force search
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

// ============================================
// Tests
// ============================================

const testing = std.testing;

test "VM basic char matching" {
    const allocator = testing.allocator;

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

    const instructions = try arena_allocator.alloc(Instruction, 5);
    instructions[0] = .{ .Save = 0 };
    instructions[1] = .{ .Char = 'a' };
    instructions[2] = .{ .Char = 'b' };
    instructions[3] = .{ .Save = 1 };
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

    const result1 = try vm.execute(compiled, "word", 0);
    try testing.expect(result1 != null);
    if (result1) |m| m.deinit(allocator);

    const result2 = try vm.execute(compiled, "sword", 0);
    try testing.expect(result2 == null);
}

test "VM backreference" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    const instructions = try arena_allocator.alloc(Instruction, 5);
    instructions[0] = .{ .Save = 0 };
    instructions[1] = .{ .Char = 'a' };
    instructions[2] = .{ .Save = 1 };
    instructions[3] = .{ .Backref = 0 };
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

    const result2 = try vm.execute(compiled, "ab", 0);
    try testing.expect(result2 == null);
}

test "VM character class" {
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

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

test "VM thread pool basic operations" {
    const allocator = testing.allocator;

    var pool = try ThreadPool.init(allocator, 10, 2);
    defer pool.deinit();

    const id1 = try pool.allocThread(0, 0);
    try testing.expectEqual(@as(ThreadId, 0), id1);

    const id2 = try pool.allocThread(1, 5);
    try testing.expectEqual(@as(ThreadId, 1), id2);

    const t1 = pool.get(id1);
    try testing.expectEqual(@as(u32, 0), t1.pc);
    try testing.expectEqual(@as(u32, 0), t1.sp);

    pool.freeThread(id1);
    const id3 = try pool.allocThread(2, 10);
    try testing.expectEqual(@as(ThreadId, 0), id3);

    const t3 = pool.get(id3);
    try testing.expectEqual(@as(u32, 2), t3.pc);
    try testing.expectEqual(@as(u32, 10), t3.sp);
}

test "VM thread pool clone" {
    const allocator = testing.allocator;

    var pool = try ThreadPool.init(allocator, 5, 1);
    defer pool.deinit();

    const id1 = try pool.allocThread(0, 0);
    pool.setSaved(id1, 0, 10);
    pool.setSaved(id1, 1, 20);

    const id2 = try pool.allocThreadClone(1, 5, id1);

    const t2 = pool.get(id2);
    try testing.expectEqual(@as(u32, 1), t2.pc);
    try testing.expectEqual(@as(u32, 5), t2.sp);

    const saved2 = pool.getSaved(id2);
    try testing.expectEqual(@as(?usize, 10), saved2[0]);
    try testing.expectEqual(@as(?usize, 20), saved2[1]);

    // Modifying id2 should not affect id1
    pool.setSaved(id2, 0, 30);
    const saved1 = pool.getSaved(id1);
    try testing.expectEqual(@as(?usize, 10), saved1[0]);
    try testing.expectEqual(@as(?usize, 30), saved2[0]);
}

test "VM circular queue" {
    const allocator = testing.allocator;

    var queue = try ThreadQueue.init(allocator, 4);
    defer queue.deinit();

    try testing.expect(queue.isEmpty());

    try queue.push(10);
    try queue.push(20);
    try queue.push(30);

    try testing.expectEqual(@as(?ThreadId, 10), queue.pop());
    try queue.push(40);

    try testing.expectEqual(@as(?ThreadId, 20), queue.pop());
    try testing.expectEqual(@as(?ThreadId, 30), queue.pop());
    try testing.expectEqual(@as(?ThreadId, 40), queue.pop());
    try testing.expectEqual(@as(?ThreadId, null), queue.pop());
}

test "VM visited set" {
    const allocator = testing.allocator;

    var visited = try VisitedSet.init(allocator, 10, 100);
    defer visited.deinit();

    try testing.expect(!visited.contains(5, 50));

    visited.add(5, 50);
    try testing.expect(visited.contains(5, 50));
    try testing.expect(!visited.contains(5, 51));
    try testing.expect(!visited.contains(6, 50));

    visited.add(5, 60);
    try testing.expect(visited.contains(5, 60));

    // Test clear via version increment
    visited.clear();
    try testing.expect(!visited.contains(5, 50));
    
    // Can add again after clear
    visited.add(5, 50);
    try testing.expect(visited.contains(5, 50));
}
