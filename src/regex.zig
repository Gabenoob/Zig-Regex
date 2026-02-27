//! Main public API for the Zig regex library
//!
//! This module provides the high-level `Regex` type for compiling patterns
//! and performing match operations.

const std = @import("std");
const parser_mod = @import("parser.zig");
const compiler_mod = @import("compiler.zig");
const vm_mod = @import("vm.zig");
const match_mod = @import("match.zig");

pub const Match = match_mod.Match;
pub const Group = match_mod.Group;
pub const SubResult = match_mod.SubResult;
pub const RegexFlags = match_mod.RegexFlags;
pub const RegexError = match_mod.RegexError;

// Re-export parser error as RegexError for compatibility
const ParserError = parser_mod.ParserError;

/// Internal compiled regex representation
const InternalCompiledRegex = struct {
    compiled: compiler_mod.CompiledRegex,

    fn deinit(self: *InternalCompiledRegex) void {
        self.compiled.deinit();
    }
};

/// Convert parser.Node to compiler.ASTNode
/// These have different structures and need adaptation
fn convertNode(allocator: std.mem.Allocator, parser_node: *parser_mod.Node) error{OutOfMemory}!compiler_mod.ASTNode {
    switch (parser_node.*) {
        .Literal => |c| {
            return compiler_mod.ASTNode{ .Literal = c };
        },
        .Any => {
            return compiler_mod.ASTNode.Any;
        },
        .Concat => |c| {
            // Parser uses binary tree for concat, compiler expects array
            // For a simple binary concat, convert both sides into a 2-element array
            const children = try allocator.alloc(compiler_mod.ASTNode, 2);
            children[0] = try convertNode(allocator, c.left);
            children[1] = try convertNode(allocator, c.right);
            return compiler_mod.ASTNode{ .Concat = children };
        },
        .Alternate => |a| {
            const left = try convertNodePtr(allocator, a.left);
            const right = try convertNodePtr(allocator, a.right);
            return compiler_mod.ASTNode{ .Alternate = .{ .left = left, .right = right } };
        },
        .Quantifier => |q| {
            const child = try convertNodePtr(allocator, q.child);

            // Convert max: parser uses maxInt(usize) for infinity, compiler uses null
            const max: ?u32 = if (q.max == std.math.maxInt(usize)) null else @intCast(q.max);

            return compiler_mod.ASTNode{ .Quantifier = .{
                .child = child,
                .min = @intCast(q.min),
                .max = max,
                .greedy = q.greedy,
            } };
        },
        .Group => |g| {
            const child = try convertNodePtr(allocator, g.child);
            // Non-capturing groups have index 0 in parser
            const index: ?u32 = if (g.index == 0) null else g.index;
            return compiler_mod.ASTNode{ .Group = .{ .child = child, .index = index } };
        },
        .CharacterClass => |cc| {
            // Convert ranges to bitmap
            var bitmap = compiler_mod.CharacterClassBitmap.init();
            for (cc.ranges) |range| {
                bitmap.setRange(range.start, range.end);
            }
            return compiler_mod.ASTNode{ .Class = .{ .bitmap = bitmap, .negated = cc.negated } };
        },
        .Anchor => |a| {
            switch (a) {
                .StartLine => return compiler_mod.ASTNode.AnchorLineStart,
                .EndLine => return compiler_mod.ASTNode.AnchorLineEnd,
                .WordBoundary => return compiler_mod.ASTNode{ .WordBoundary = true },
                .NotWordBoundary => return compiler_mod.ASTNode{ .WordBoundary = false },
            }
        },
        .Backreference => |b| {
            return compiler_mod.ASTNode{ .Backref = b };
        },
    }
}

/// Helper to convert a node and return an allocated pointer
fn convertNodePtr(allocator: std.mem.Allocator, parser_node: *parser_mod.Node) error{OutOfMemory}!*compiler_mod.ASTNode {
    const ptr = try allocator.create(compiler_mod.ASTNode);
    ptr.* = try convertNode(allocator, parser_node);
    return ptr;
}

/// Free a converted AST node tree
fn freeASTNode(allocator: std.mem.Allocator, node: *compiler_mod.ASTNode) void {
    switch (node.*) {
        .Concat => |children| {
            // Recursively free children first, then free the array
            for (children) |*child| {
                freeASTNode(allocator, child);
            }
            allocator.free(children);
        },
        .Alternate => |alt| {
            freeASTNode(allocator, alt.left);
            allocator.destroy(alt.left);
            freeASTNode(allocator, alt.right);
            allocator.destroy(alt.right);
        },
        .Quantifier => |q| {
            freeASTNode(allocator, q.child);
            allocator.destroy(q.child);
        },
        .Group => |g| {
            freeASTNode(allocator, g.child);
            allocator.destroy(g.child);
        },
        else => {},
    }
}

/// A compiled regular expression for pattern matching
///
/// This is the main type for working with regexes. Use `Regex.compile()`
/// to create a regex from a pattern string, then use methods like `match()`,
/// `search()`, or `findAll()` to perform matching operations.
///
/// ## Memory Management
///
/// - `Regex.compile()` duplicates the pattern string internally
/// - The regex must be deinitialized with `regex.deinit()` when no longer needed
/// - Match results contain allocated data and must be freed with `match.deinit()`
///
/// ## Example
/// ```zig
/// var regex = try Regex.compile(allocator, "\\w+@\\w+\\.\\w+");
/// defer regex.deinit();
///
/// if (try regex.search("Contact: user@example.com")) |*m| {
///     defer m.deinit();
///     std.debug.print("Found email: {s}\n", .{m.fullMatch()});
/// }
/// ```
pub const Regex = struct {
    /// Allocator used for all internal allocations
    allocator: std.mem.Allocator,
    /// The original pattern string (owned)
    pattern: []const u8,
    /// Compilation/matching flags
    flags: RegexFlags,
    /// Compiled bytecode (null if not yet compiled)
    compiled: ?InternalCompiledRegex,
    /// Number of capture groups (including group 0)
    capture_group_count: u32,

    /// Free all resources associated with this regex
    ///
    /// This includes the duplicated pattern string and any compiled bytecode.
    pub fn deinit(self: *Regex) void {
        self.allocator.free(self.pattern);
        if (self.compiled) |*c| {
            c.deinit();
        }
    }

    /// Compile a regex pattern with default flags
    ///
    /// The pattern is parsed and compiled to bytecode for efficient matching.
    /// The pattern string is duplicated and stored internally.
    ///
    /// ## Parameters
    /// - `allocator`: Allocator for internal resources
    /// - `pattern`: The regex pattern string
    ///
    /// ## Errors
    /// Returns `RegexError` if the pattern is invalid or out of memory.
    ///
    /// ## Example
    /// ```zig
    /// var regex = try Regex.compile(allocator, "\\d{3}-\\d{4}");
    /// defer regex.deinit();
    /// ```
    pub fn compile(allocator: std.mem.Allocator, pattern: []const u8) RegexError!Regex {
        return compileWithFlags(allocator, pattern, RegexFlags{});
    }

    /// Compile a regex pattern with specific flags
    ///
    /// ## Parameters
    /// - `allocator`: Allocator for internal resources
    /// - `pattern`: The regex pattern string
    /// - `flags`: Compilation and matching flags
    ///
    /// ## Errors
    /// Returns `RegexError` if the pattern is invalid or out of memory.
    ///
    /// ## Example
    /// ```zig
    /// var flags = RegexFlags{ .ignore_case = true };
    /// var regex = try Regex.compileWithFlags(allocator, "hello", flags);
    /// defer regex.deinit();
    ///
    /// // Will match "HELLO", "Hello", etc.
    /// _ = try regex.match("HELLO");
    /// ```
    pub fn compileWithFlags(allocator: std.mem.Allocator, pattern: []const u8, flags: RegexFlags) RegexError!Regex {
        // Store the pattern
        const pattern_copy = try allocator.dupe(u8, pattern);
        errdefer allocator.free(pattern_copy);

        // Parse the pattern
        const ast = parser_mod.parse(pattern, allocator) catch |err| {
            switch (err) {
                error.InvalidPattern => return RegexError.InvalidPattern,
                error.UnmatchedParenthesis => return RegexError.UnmatchedParenthesis,
                error.UnmatchedBracket => return RegexError.UnmatchedBracket,
                error.InvalidEscape => return RegexError.InvalidEscape,
                error.InvalidGroup => return RegexError.InvalidGroup,
                error.InvalidRange => return RegexError.InvalidRange,
                error.InvalidQuantifier => return RegexError.InvalidQuantifier,
                error.OutOfMemory => return RegexError.OutOfMemory,
            }
        };
        defer ast.deinit(allocator); // deinit already destroys the root node

        // Convert parser AST to compiler AST
        var compiler_ast = try convertNode(allocator, ast);
        defer freeASTNode(allocator, &compiler_ast);

        // Compile to bytecode
        const compiled_regex = compiler_mod.compile(allocator, compiler_ast) catch |err| {
            switch (err) {
                error.OutOfMemory => return RegexError.OutOfMemory,
            }
        };

        // Count capture groups
        const group_count = parser_mod.countGroups(pattern) catch 0;

        return Regex{
            .allocator = allocator,
            .pattern = pattern_copy,
            .flags = flags,
            .compiled = .{ .compiled = compiled_regex },
            .capture_group_count = group_count + 1, // +1 for group 0 (full match)
        };
    }

    /// Match the pattern at the beginning of text only
    ///
    /// This is equivalent to having an implicit `^` anchor at the start.
    /// The pattern must match starting at the first character.
    ///
    /// ## Parameters
    /// - `text`: The text to match against
    ///
    /// ## Returns
    /// A `Match` if successful, null otherwise. The match must be deinitialized.
    ///
    /// ## Example
    /// ```zig
    /// var regex = try Regex.compile(allocator, "hello");
    /// defer regex.deinit();
    ///
    /// // Matches because "hello" is at the start
    /// _ = try regex.match("hello world");
    ///
    /// // Does not match because "hello" is not at the start
    /// _ = try regex.match("say hello"); // returns null
    /// ```
    pub fn match(self: *const Regex, text: []const u8) RegexError!?Match {
        if (self.compiled == null) return null;

        // Use VM to execute starting at position 0
        var vm = vm_mod.VM.init(self.allocator, .{
            .ignore_case = self.flags.ignore_case,
            .multiline = self.flags.multiline,
            .dotall = self.flags.dotall,
        });

        const result = try vm.execute(self.compiled.?.compiled, text, 0);

        if (result) |vm_match| {
            defer vm_match.deinit(self.allocator);

            // Convert VM MatchResult to our Match type
            const groups = try self.allocator.alloc(?Group, vm_match.groups.len);
            for (vm_match.groups, 0..) |vm_group, i| {
                if (vm_group) |g| {
                    groups[i] = Group{
                        .start = g.start.?,
                        .end = g.end.?,
                        .name = null, // TODO: track named groups
                    };
                } else {
                    groups[i] = null;
                }
            }

            return Match{
                .text = text,
                .start = vm_match.start,
                .end = vm_match.end,
                .groups = groups,
                .allocator = self.allocator,
            };
        }

        return null;
    }

    /// Search for a match anywhere in the text
    ///
    /// Unlike `match()`, this scans through the text looking for the pattern
    /// at any position.
    ///
    /// ## Parameters
    /// - `text`: The text to search
    ///
    /// ## Returns
    /// A `Match` if found, null otherwise. The match must be deinitialized.
    ///
    /// ## Example
    /// ```zig
    /// var regex = try Regex.compile(allocator, "\\d+");
    /// defer regex.deinit();
    ///
    /// if (try regex.search("abc123def")) |*m| {
    ///     defer m.deinit();
    ///     std.debug.print("Found: {s}\n", .{m.fullMatch()}); // "123"
    /// }
    /// ```
    pub fn search(self: *const Regex, text: []const u8) RegexError!?Match {
        if (self.compiled == null) return null;

        const result = try vm_mod.search(self.allocator, self.compiled.?.compiled, text, .{
            .ignore_case = self.flags.ignore_case,
            .multiline = self.flags.multiline,
            .dotall = self.flags.dotall,
        });

        if (result) |vm_match| {
            defer vm_match.deinit(self.allocator);

            // Convert VM MatchResult to our Match type
            const groups = try self.allocator.alloc(?Group, vm_match.groups.len);
            for (vm_match.groups, 0..) |vm_group, i| {
                if (vm_group) |g| {
                    groups[i] = Group{
                        .start = g.start.?,
                        .end = g.end.?,
                        .name = null,
                    };
                } else {
                    groups[i] = null;
                }
            }

            return Match{
                .text = text,
                .start = vm_match.start,
                .end = vm_match.end,
                .groups = groups,
                .allocator = self.allocator,
            };
        }

        return null;
    }

    /// Find all non-overlapping matches in the text
    ///
    /// Returns a slice of `Match` objects. Each match must be deinitialized,
    /// and the slice itself must be freed with `allocator.free(matches)`.
    ///
    /// ## Parameters
    /// - `text`: The text to search
    ///
    /// ## Returns
    /// A slice of matches (may be empty). Caller owns the returned memory.
    ///
    /// ## Example
    /// ```zig
    /// var regex = try Regex.compile(allocator, "\\w+");
    /// defer regex.deinit();
    ///
    /// const matches = try regex.findAll("hello world");
    /// defer {
    ///     for (matches) |*m| m.deinit();
    ///     allocator.free(matches);
    /// }
    ///
    /// for (matches) |m| {
    ///     std.debug.print("Word: {s}\n", .{m.fullMatch()});
    /// }
    /// ```
    pub fn findAll(self: *const Regex, text: []const u8) RegexError![]Match {
        if (self.compiled == null) return &[_]Match{};

        const results = try vm_mod.findAll(self.allocator, self.compiled.?.compiled, text, .{
            .ignore_case = self.flags.ignore_case,
            .multiline = self.flags.multiline,
            .dotall = self.flags.dotall,
        });

        // Convert VM MatchResult array to our Match array
        var matches = try self.allocator.alloc(Match, results.len);
        errdefer {
            for (matches) |*m| m.deinit();
            self.allocator.free(matches);
        }

        for (results, 0..) |vm_match, i| {
            const groups = try self.allocator.alloc(?Group, vm_match.groups.len);
            for (vm_match.groups, 0..) |vm_group, j| {
                if (vm_group) |g| {
                    groups[j] = Group{
                        .start = g.start.?,
                        .end = g.end.?,
                        .name = null,
                    };
                } else {
                    groups[j] = null;
                }
            }

            // We need to duplicate the groups array since vm_match will be freed
            matches[i] = Match{
                .text = text,
                .start = vm_match.start,
                .end = vm_match.end,
                .groups = groups,
                .allocator = self.allocator,
            };

            // Free the VM's copy of groups (we made our own)
            self.allocator.free(vm_match.groups);
        }

        // Free the results array itself (but not items since we already freed groups)
        self.allocator.free(results);

        return matches;
    }

    /// Replace the first match with replacement text
    ///
    /// Returns an allocated string with the replacement made.
    /// If no match is found, returns a copy of the original text.
    ///
    /// ## Parameters
    /// - `replacement`: The text to replace the match with
    /// - `text`: The original text
    ///
    /// ## Returns
    /// An allocated string. Caller owns the returned memory.
    ///
    /// ## Example
    /// ```zig
    /// var regex = try Regex.compile(allocator, "cat");
    /// defer regex.deinit();
    ///
    /// const result = try regex.sub("dog", "the cat sat");
    /// defer allocator.free(result);
    /// // result == "the dog sat"
    /// ```
    pub fn sub(self: *const Regex, replacement: []const u8, text: []const u8) RegexError![]u8 {
        var result: std.ArrayList(u8) = .empty;
        defer result.deinit(self.allocator);

        if (try self.search(text)) |match_result| {
            // Need to make a mutable copy since deinit requires *Match
            var mutable_match = match_result;
            defer mutable_match.deinit();

            try result.appendSlice(self.allocator, text[0..mutable_match.start]);
            try result.appendSlice(self.allocator, replacement);
            try result.appendSlice(self.allocator, text[mutable_match.end..]);

            return result.toOwnedSlice(self.allocator);
        } else {
            // No match, return copy of original
            return self.allocator.dupe(u8, text);
        }
    }

    /// Replace all matches with replacement text
    ///
    /// Similar to `sub()` but replaces every non-overlapping occurrence.
    ///
    /// ## Parameters
    /// - `replacement`: The text to replace each match with
    /// - `text`: The original text
    ///
    /// ## Returns
    /// An allocated string. Caller owns the returned memory.
    ///
    /// ## Example
    /// ```zig
    /// var regex = try Regex.compile(allocator, "a");
    /// defer regex.deinit();
    ///
    /// const result = try regex.subAll("o", "banana");
    /// defer allocator.free(result);
    /// // result == "bonono"
    /// ```
    pub fn subAll(self: *const Regex, replacement: []const u8, text: []const u8) RegexError![]u8 {
        const matches = try self.findAll(text);
        defer {
            for (matches) |*m| m.deinit();
            self.allocator.free(matches);
        }

        if (matches.len == 0) {
            return self.allocator.dupe(u8, text);
        }

        var result: std.ArrayList(u8) = .empty;
        defer result.deinit(self.allocator);

        var last_end: usize = 0;
        for (matches) |m| {
            try result.appendSlice(self.allocator, text[last_end..m.start]);
            try result.appendSlice(self.allocator, replacement);
            last_end = m.end;
        }
        try result.appendSlice(self.allocator, text[last_end..]);

        return result.toOwnedSlice(self.allocator);
    }

    /// Split the text by regex matches
    ///
    /// Returns a slice of strings split by the pattern. The matches themselves
    /// are not included in the result.
    ///
    /// ## Parameters
    /// - `text`: The text to split
    ///
    /// ## Returns
    /// A slice of strings. Caller owns the returned memory (both slice and contents).
    ///
    /// ## Example
    /// ```zig
    /// var regex = try Regex.compile(allocator, "\\s+");
    /// defer regex.deinit();
    ///
    /// const parts = try regex.split("hello   world  test");
    /// defer {
    ///     for (parts) |p| allocator.free(p);
    ///     allocator.free(parts);
    /// }
    /// // parts == .{ "hello", "world", "test" }
    /// ```
    pub fn split(self: *const Regex, text: []const u8) RegexError![][]u8 {
        const matches = try self.findAll(text);
        defer {
            for (matches) |*m| m.deinit();
            self.allocator.free(matches);
        }

        var parts: std.ArrayList([]u8) = .empty;
        defer parts.deinit(self.allocator);

        var last_end: usize = 0;
        for (matches) |m| {
            const part = try self.allocator.dupe(u8, text[last_end..m.start]);
            try parts.append(self.allocator, part);
            last_end = m.end;
        }

        // Add remaining text
        const final_part = try self.allocator.dupe(u8, text[last_end..]);
        try parts.append(self.allocator, final_part);

        return parts.toOwnedSlice(self.allocator);
    }

    /// Returns the number of capture groups (including group 0)
    ///
    /// Group 0 always represents the full match.
    pub fn groupCount(self: Regex) usize {
        return self.capture_group_count;
    }
};

/// Escape special regex characters in a string
///
/// Escapes the following characters with a backslash:
/// `\`, `^`, `$`, `.`, `|`, `?`, `*`, `+`, `(`, `)`, `[`, `]`, `{`, `}`
///
/// ## Parameters
/// - `allocator`: Allocator for the result string
/// - `text`: The text to escape
///
/// ## Returns
/// An allocated escaped string. Caller owns the returned memory.
///
/// ## Example
/// ```zig
/// const escaped = try escape(allocator, "hello.world");
/// defer allocator.free(escaped);
/// // escaped == "hello\\.world"
/// ```
pub fn escape(allocator: std.mem.Allocator, text: []const u8) RegexError![]u8 {
    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    for (text) |c| {
        switch (c) {
            '\\', '^', '$', '.', '|', '?', '*', '+', '(', ')', '[', ']', '{', '}' => {
                try result.append(allocator, '\\');
                try result.append(allocator, c);
            },
            else => try result.append(allocator, c),
        }
    }

    return result.toOwnedSlice(allocator);
}

const testing = std.testing;

test "escape function" {
    const allocator = testing.allocator;

    const escaped1 = try escape(allocator, "hello.world");
    defer allocator.free(escaped1);
    try testing.expectEqualStrings("hello\\.world", escaped1);
}

test "RegexFlags fromString" {
    const flags = RegexFlags.fromString("ims");
    try testing.expect(flags.ignore_case);
}

test "Regex simple literal match" {
    const allocator = testing.allocator;

    var re = try Regex.compile(allocator, "hello");
    defer re.deinit();

    const m = try re.match("hello world");
    try testing.expect(m != null);
    if (m) |match| {
        var mutable = match;
        defer mutable.deinit();
        try testing.expectEqualStrings("hello", mutable.fullMatch());
    }
}

test "Regex character class" {
    const allocator = testing.allocator;

    var re = try Regex.compile(allocator, "[0-9]+");
    defer re.deinit();

    const m = try re.search("abc123def");
    try testing.expect(m != null);
    if (m) |match| {
        var mutable = match;
        defer mutable.deinit();
        try testing.expectEqualStrings("123", mutable.fullMatch());
    }
}

test "Regex capture groups" {
    const allocator = testing.allocator;

    var re = try Regex.compile(allocator, "(\\w+)=(\\d+)");
    defer re.deinit();

    const m = try re.match("age=25");
    try testing.expect(m != null);
    if (m) |match| {
        var mutable = match;
        defer mutable.deinit();
        try testing.expectEqualStrings("age=25", mutable.fullMatch());
        try testing.expectEqualStrings("age", mutable.get(1).?);
        try testing.expectEqualStrings("25", mutable.get(2).?);
    }
}

test "Regex findAll" {
    const allocator = testing.allocator;

    var re = try Regex.compile(allocator, "\\w+");
    defer re.deinit();

    const matches = try re.findAll("hello world test");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    try testing.expectEqual(@as(usize, 3), matches.len);
    try testing.expectEqualStrings("hello", matches[0].fullMatch());
    try testing.expectEqualStrings("world", matches[1].fullMatch());
    try testing.expectEqualStrings("test", matches[2].fullMatch());
}
