const std = @import("std");

/// Regular expression compilation and matching flags
pub const RegexFlags = packed struct {
    ignore_case: bool = false, // i flag
    multiline: bool = false, // m flag (^/$ match line boundaries)
    dotall: bool = false, // s flag (. matches newlines)
    verbose: bool = false, // x flag (whitespace ignored)
    unicode: bool = true, // u flag (unicode support)

    pub fn fromString(str: []const u8) RegexFlags {
        var flags = RegexFlags{};
        for (str) |c| {
            switch (c) {
                'i' => flags.ignore_case = true,
                'm' => flags.multiline = true,
                's' => flags.dotall = true,
                'x' => flags.verbose = true,
                'u' => flags.unicode = true,
                else => {},
            }
        }
        return flags;
    }
};

/// Error types for regex operations
pub const RegexError = error{
    InvalidPattern,
    UnmatchedParenthesis,
    UnmatchedBracket,
    InvalidEscape,
    InvalidGroup,
    InvalidRange,
    InvalidQuantifier,
    InvalidBackreference,
    OutOfMemory,
};

/// Represents a capture group within a match
pub const Group = struct {
    start: usize,
    end: usize,
    name: ?[]const u8,

    pub fn init(start: usize, end: usize) Group {
        return .{
            .start = start,
            .end = end,
            .name = null,
        };
    }

    pub fn initNamed(start: usize, end: usize, name: []const u8) Group {
        return .{
            .start = start,
            .end = end,
            .name = name,
        };
    }

    /// Returns true if this group participated in the match
    pub fn matched(self: *const Group) bool {
        return self.start <= self.end;
    }

    /// Returns the length of the matched group
    pub fn len(self: *const Group) usize {
        if (self.start > self.end) return 0;
        return self.end - self.start;
    }
};

/// Result of a successful regex match operation
pub const Match = struct {
    text: []const u8, // Original text
    start: usize, // Match start position
    end: usize, // Match end position
    groups: []?Group, // Capture groups (index 0 = full match)
    allocator: std.mem.Allocator,

    /// Get the matched text for a capture group by index
    /// Returns null if the group did not participate in the match
    pub fn get(self: *const Match, group_index: usize) ?[]const u8 {
        if (group_index >= self.groups.len) return null;
        const group = self.groups[group_index] orelse return null;
        if (group.start > group.end) return null; // Unmatched optional group
        return self.text[group.start..group.end];
    }

    /// Get the matched text for a named capture group
    /// Returns null if the named group doesn't exist or didn't participate
    pub fn getNamed(self: *const Match, name: []const u8) ?[]const u8 {
        for (self.groups) |maybe_group| {
            if (maybe_group) |group| {
                if (group.name) |group_name| {
                    if (std.mem.eql(u8, group_name, name)) {
                        if (group.start > group.end) return null;
                        return self.text[group.start..group.end];
                    }
                }
            }
        }
        return null;
    }

    /// Get the span (start, end) of the full match
    pub fn span(self: *const Match) struct { usize, usize } {
        return .{ self.start, self.end };
    }

    /// Get a span for a specific group by index
    pub fn groupSpan(self: *const Match, group_index: usize) ?struct { usize, usize } {
        if (group_index >= self.groups.len) return null;
        const group = self.groups[group_index] orelse return null;
        if (group.start > group.end) return null;
        return .{ group.start, group.end };
    }

    /// Get all capture groups as a slice of optional strings
    /// Caller owns the returned slice (must free with self.allocator)
    pub fn groupsSlice(self: *const Match, allocator: std.mem.Allocator) RegexError![]?[]const u8 {
        var result = try allocator.alloc(?[]const u8, self.groups.len);
        errdefer allocator.free(result);

        for (self.groups, 0..) |maybe_group, i| {
            if (maybe_group) |group| {
                if (group.start > group.end) {
                    result[i] = null;
                } else {
                    result[i] = self.text[group.start..group.end];
                }
            } else {
                result[i] = null;
            }
        }

        return result;
    }

    /// Get all named groups as a hash map of name -> matched text
    /// Caller owns the returned map and must deinit it
    pub fn namedGroups(self: *const Match, allocator: std.mem.Allocator) RegexError!std.StringHashMap([]const u8) {
        var map = std.StringHashMap([]const u8).init(allocator);
        errdefer map.deinit();

        for (self.groups) |maybe_group| {
            if (maybe_group) |group| {
                if (group.name) |name| {
                    if (group.start <= group.end) {
                        try map.put(name, self.text[group.start..group.end]);
                    }
                }
            }
        }

        return map;
    }

    /// Returns the full matched text
    pub fn fullMatch(self: *const Match) []const u8 {
        return self.text[self.start..self.end];
    }

    /// Returns the length of the match
    pub fn len(self: *const Match) usize {
        return self.end - self.start;
    }

    /// Returns the number of capture groups (including group 0)
    pub fn groupCount(self: *const Match) usize {
        return self.groups.len;
    }

    /// Release all resources associated with this match
    pub fn deinit(self: *const Match) void {
        self.allocator.free(self.groups);
    }
};

/// Result of subn operation containing replacement text and count
pub const SubResult = struct {
    result: []u8,
    count: usize,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *SubResult) void {
        self.allocator.free(self.result);
    }
};

/// Internal: Represents the state of a match during VM execution
pub const MatchState = struct {
    capture_groups: []?Group,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, num_groups: usize) RegexError!MatchState {
        const groups = try allocator.alloc(?Group, num_groups);
        @memset(groups, null);
        return .{
            .capture_groups = groups,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *MatchState) void {
        self.allocator.free(self.capture_groups);
    }

    pub fn setGroup(self: *MatchState, index: usize, start: usize, end: usize) void {
        if (index < self.capture_groups.len) {
            self.capture_groups[index] = Group.init(start, end);
        }
    }

    pub fn setGroupNamed(self: *MatchState, index: usize, start: usize, end: usize, name: []const u8) void {
        if (index < self.capture_groups.len) {
            self.capture_groups[index] = Group.initNamed(start, end, name);
        }
    }

    pub fn getGroup(self: *const MatchState, index: usize) ?Group {
        if (index >= self.capture_groups.len) return null;
        return self.capture_groups[index];
    }

    /// Create a Match result from this state
    pub fn toMatch(self: *const MatchState, text: []const u8, start: usize, end: usize) RegexError!Match {
        // Copy capture groups
        const groups_copy = try self.allocator.alloc(?Group, self.capture_groups.len);
        errdefer self.allocator.free(groups_copy);

        for (self.capture_groups, 0..) |maybe_group, i| {
            if (maybe_group) |group| {
                groups_copy[i] = group;
            } else {
                groups_copy[i] = null;
            }
        }

        return Match{
            .text = text,
            .start = start,
            .end = end,
            .groups = groups_copy,
            .allocator = self.allocator,
        };
    }
};

/// Internal: Information about capture groups for the compiler/VM
pub const CaptureGroupInfo = struct {
    name: ?[]const u8,
    index: usize,
    is_optional: bool,
};

/// Internal: Registry for tracking named capture groups
pub const GroupRegistry = struct {
    groups: std.StringHashMap(usize),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) GroupRegistry {
        return .{
            .groups = std.StringHashMap(usize).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GroupRegistry) void {
        // Note: We don't free the names here as they may be referenced elsewhere
        self.groups.deinit();
    }

    pub fn register(self: *GroupRegistry, name: []const u8, index: usize) RegexError!void {
        try self.groups.put(name, index);
    }

    pub fn getIndex(self: *const GroupRegistry, name: []const u8) ?usize {
        return self.groups.get(name);
    }

    pub fn count(self: *const GroupRegistry) usize {
        return self.groups.count();
    }
};

/// Internal: Result type for VM thread matching
pub const ThreadResult = union(enum) {
    match: Match,
    fail: void,
    split: struct { pc: usize, pos: usize }, // For backtracking
};

/// Internal: VM thread state for NFA simulation
pub const Thread = struct {
    pc: usize, // Program counter
    pos: usize, // Position in input
    state: MatchState,

    pub fn init(pc: usize, pos: usize, state: MatchState) Thread {
        return .{
            .pc = pc,
            .pos = pos,
            .state = state,
        };
    }

    pub fn clone(self: *const Thread, allocator: std.mem.Allocator) RegexError!Thread {
        const new_groups = try allocator.alloc(?Group, self.state.capture_groups.len);
        errdefer allocator.free(new_groups);

        @memcpy(new_groups, self.state.capture_groups);

        return .{
            .pc = self.pc,
            .pos = self.pos,
            .state = .{
                .capture_groups = new_groups,
                .allocator = allocator,
            },
        };
    }

    pub fn deinit(self: *Thread) void {
        self.state.deinit();
    }
};

/// Internal: Position in the input text
pub const Position = struct {
    index: usize,
    line: usize,
    column: usize,

    pub fn init(index: usize) Position {
        return .{
            .index = index,
            .line = 1,
            .column = index + 1,
        };
    }

    pub fn advance(self: *Position, char: u8) void {
        self.index += 1;
        if (char == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
};

/// Internal: Match options for fine-tuning matching behavior
pub const MatchOptions = struct {
    start_pos: usize = 0,
    end_pos: ?usize = null,
    anchored: bool = false, // If true, ^ is implicit at start_pos
};

/// Internal: Track multiple matches for findAll operations
pub const MatchCollection = struct {
    matches: std.ArrayList(Match),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) MatchCollection {
        return .{
            .matches = std.ArrayList(Match).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *MatchCollection) void {
        // Note: Individual matches must be deinit'd separately
        self.matches.deinit();
    }

    pub fn add(self: *MatchCollection, match: Match) RegexError!void {
        try self.matches.append(match);
    }

    pub fn toSlice(self: *const MatchCollection) []Match {
        return self.matches.items;
    }

    pub fn count(self: *const MatchCollection) usize {
        return self.matches.items.len;
    }
};
