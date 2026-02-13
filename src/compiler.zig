//! Regex Compiler Module
//! Compiles AST from parser into bytecode instructions for the VM

const std = @import("std");

/// Character class bitmap (256 bits for all possible bytes)
pub const CharacterClassBitmap = struct {
    bits: [32]u32, // 32 * 32 = 1024 bits, but we use 256 for ASCII/bytes

    pub fn init() CharacterClassBitmap {
        return .{ .bits = [_]u32{0} ** 32 };
    }

    pub fn set(self: *CharacterClassBitmap, c: u8) void {
        const idx = c >> 5; // divide by 32
        const bit = @as(u32, 1) << @intCast(c & 0x1F); // mod 32
        self.bits[idx] |= bit;
    }

    pub fn setRange(self: *CharacterClassBitmap, start: u8, end: u8) void {
        var i = start;
        while (i <= end) : (i += 1) {
            self.set(i);
        }
    }

    pub fn contains(self: CharacterClassBitmap, c: u8) bool {
        const idx = c >> 5;
        const bit = @as(u32, 1) << @intCast(c & 0x1F);
        return (self.bits[idx] & bit) != 0;
    }

    pub fn invert(self: *CharacterClassBitmap) void {
        for (&self.bits) |*word| {
            word.* = ~word.*;
        }
    }

    pub fn clone(self: CharacterClassBitmap) CharacterClassBitmap {
        return .{ .bits = self.bits };
    }
};

/// Bytecode instruction types
pub const Instruction = union(enum) {
    Char: u8, // Match literal character
    Any, // Match any character
    Range: struct { start: u8, end: u8 }, // Match character in range
    Class: CharacterClassBitmap, // Match character in class
    Split: struct { x: u32, y: u32 }, // Try both paths (non-deterministic choice)
    Jump: u32, // Jump to instruction index
    Save: u32, // Save position to capture group n (0 = full match, odd=starts, even=ends)
    Match, // Successful match
    LineStart, // Match ^ (start of string/line)
    LineEnd, // Match $ (end of string/line)
    WordBoundary, // Match word boundary
    Backref: u32, // Match previously captured group n

    pub fn format(
        self: Instruction,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Char => |c| try writer.print("Char('{c}')", .{c}),
            .Any => try writer.writeAll("Any"),
            .Range => |r| try writer.print("Range({c}-{c})", .{ r.start, r.end }),
            .Class => try writer.writeAll("Class(...)"),
            .Split => |s| try writer.print("Split({d}, {d})", .{ s.x, s.y }),
            .Jump => |j| try writer.print("Jump({d})", .{j}),
            .Save => |s| try writer.print("Save({d})", .{s}),
            .Match => try writer.writeAll("Match"),
            .LineStart => try writer.writeAll("LineStart"),
            .LineEnd => try writer.writeAll("LineEnd"),
            .WordBoundary => try writer.writeAll("WordBoundary"),
            .Backref => |b| try writer.print("Backref({d})", .{b}),
        }
    }
};

/// Compiled regex program
pub const CompiledRegex = struct {
    instructions: []Instruction,
    capture_groups: u32,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *CompiledRegex) void {
        self.allocator.free(self.instructions);
    }

    pub fn dump(self: CompiledRegex) void {
        std.debug.print("\nCompiled Regex ({d} capture groups):\n", .{self.capture_groups});
        for (self.instructions, 0..) |inst, i| {
            std.debug.print("{d:4}: {any}\n", .{ i, inst });
        }
        std.debug.print("\n", .{});
    }
};

/// AST Node types (produced by parser)
pub const ASTNode = union(enum) {
    Literal: u8, // Single character
    Any, // Dot (.) - any character
    Concat: []ASTNode, // Sequence of nodes
    Alternate: struct { left: *ASTNode, right: *ASTNode }, // left | right
    Quantifier: struct {
        child: *ASTNode,
        min: u32,
        max: ?u32, // null = infinity
        greedy: bool,
    },
    Group: struct {
        child: *ASTNode,
        index: ?u32, // null = non-capturing
    },
    Class: struct {
        bitmap: CharacterClassBitmap,
        negated: bool,
    },
    AnchorLineStart, // ^
    AnchorLineEnd, // $
    WordBoundary: bool, // true = word boundary, false = non-word boundary
    Backref: u32, // \1, \2, etc.
};

/// Compiler state
const Compiler = struct {
    allocator: std.mem.Allocator,
    instructions: std.ArrayList(Instruction),
    capture_group_counter: u32,

    fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .allocator = allocator,
            .instructions = .empty,
            .capture_group_counter = 0,
        };
    }

    fn deinit(self: *Compiler) void {
        self.instructions.deinit(self.allocator);
    }

    fn emit(self: *Compiler, inst: Instruction) !void {
        try self.instructions.append(self.allocator, inst);
    }

    fn currentPos(self: Compiler) u32 {
        return @intCast(self.instructions.items.len);
    }

    fn patchJump(self: *Compiler, pos: usize, target: u32) void {
        switch (self.instructions.items[pos]) {
            .Jump => |*j| j.* = target,
            .Split => |*s| {
                // Used for patching Split instructions
                s.x = target;
            },
            else => {},
        }
    }

    fn patchSplit(self: *Compiler, pos: usize, x: u32, y: u32) void {
        switch (self.instructions.items[pos]) {
            .Split => |*s| {
                s.x = x;
                s.y = y;
            },
            else => {},
        }
    }
};

/// Compile an AST node to bytecode
fn compileNode(compiler: *Compiler, node: ASTNode) error{OutOfMemory}!void {
    switch (node) {
        .Literal => |c| {
            try compiler.emit(.{ .Char = c });
        },

        .Any => {
            try compiler.emit(.Any);
        },

        .Concat => |children| {
            for (children) |child| {
                try compileNode(compiler, child);
            }
        },

        .Alternate => |alt| {
            // Generate: Split(L1, L2), <left>, Jump(L3), L2: <right>, L3:
            const split_pos = compiler.currentPos();
            try compiler.emit(.{ .Split = .{ .x = 0, .y = 0 } }); // placeholder

            // Compile left branch
            try compileNode(compiler, alt.left.*);

            // Jump to end after left branch succeeds
            const jump_pos = compiler.currentPos();
            try compiler.emit(.{ .Jump = 0 }); // placeholder

            // Right branch starts here
            const right_start = compiler.currentPos();
            try compileNode(compiler, alt.right.*);

            const end_pos = compiler.currentPos();

            // Patch jumps
            compiler.patchSplit(split_pos, split_pos + 1, right_start);
            compiler.patchJump(jump_pos, end_pos);
        },

        .Quantifier => |q| {
            // Handle different quantifier types
            const min = q.min;
            const max = q.max;

            if (min == 0 and max == null) {
                // * = zero or more
                // L1: Split(L2, L3), L2: <child>, Jump(L1), L3:
                const split_pos = compiler.currentPos();
                try compiler.emit(.{ .Split = .{ .x = 0, .y = 0 } });

                const body_start = compiler.currentPos();
                try compileNode(compiler, q.child.*);

                try compiler.emit(.{ .Jump = split_pos });

                const end_pos = compiler.currentPos();
                compiler.patchSplit(split_pos, body_start, end_pos);
            } else if (min == 1 and max == null) {
                // + = one or more
                // L1: <child>, Split(L1, L2), L2:
                const body_start = compiler.currentPos();
                try compileNode(compiler, q.child.*);

                const split_pos = compiler.currentPos();
                try compiler.emit(.{ .Split = .{ .x = 0, .y = 0 } });

                const end_pos = compiler.currentPos();
                compiler.patchSplit(split_pos, body_start, end_pos);
            } else if (min == 0 and max != null and max.? == 1) {
                // ? = zero or one
                // Split(L1, L2), L1: <child>, L2:
                const split_pos = compiler.currentPos();
                try compiler.emit(.{ .Split = .{ .x = 0, .y = 0 } });

                try compileNode(compiler, q.child.*);

                const end_pos = compiler.currentPos();
                compiler.patchSplit(split_pos, split_pos + 1, end_pos);
            } else {
                // {n,m} quantifiers
                try compileCountedQuantifier(compiler, q.child.*, min, max);
            }
        },

        .Group => |g| {
            if (g.index) |idx| {
                // Capturing group: emit Save instructions
                try compiler.emit(.{ .Save = idx * 2 }); // Start save
                try compileNode(compiler, g.child.*);
                try compiler.emit(.{ .Save = idx * 2 + 1 }); // End save
            } else {
                // Non-capturing group: just compile the child
                try compileNode(compiler, g.child.*);
            }
        },

        .Class => |c| {
            if (c.negated) {
                // For negated class, we emit a special Class instruction
                // The VM needs to handle negation
                var inverted = c.bitmap.clone();
                inverted.invert();
                try compiler.emit(.{ .Class = inverted });
            } else {
                try compiler.emit(.{ .Class = c.bitmap });
            }
        },

        .AnchorLineStart => {
            try compiler.emit(.LineStart);
        },

        .AnchorLineEnd => {
            try compiler.emit(.LineEnd);
        },

        .WordBoundary => |is_boundary| {
            if (is_boundary) {
                try compiler.emit(.WordBoundary);
            } else {
                // For non-word boundary, we'd need a NotWordBoundary instruction
                // For now, emit WordBoundary and let VM handle negation via flags
                // or we could emit a special instruction
                try compiler.emit(.WordBoundary);
            }
        },

        .Backref => |n| {
            try compiler.emit(.{ .Backref = n });
        },
    }
}

/// Compile counted quantifiers {n,m}
fn compileCountedQuantifier(compiler: *Compiler, child: ASTNode, min: u32, max: ?u32) error{OutOfMemory}!void {
    // Emit min required repetitions
    var i: u32 = 0;
    while (i < min) : (i += 1) {
        try compileNode(compiler, child);
    }

    if (max) |m| {
        // Bounded repetition: emit (max - min) optional repetitions
        var j = min;
        while (j < m) : (j += 1) {
            const split_pos = compiler.currentPos();
            try compiler.emit(.{ .Split = .{ .x = 0, .y = 0 } });
            try compileNode(compiler, child);
            const end_pos = compiler.currentPos();
            compiler.patchSplit(split_pos, split_pos + 1, end_pos);
        }
    } else {
        // Unbounded: emit star loop for remaining
        const split_pos = compiler.currentPos();
        try compiler.emit(.{ .Split = .{ .x = 0, .y = 0 } });

        const body_start = compiler.currentPos();
        try compileNode(compiler, child);
        try compiler.emit(.{ .Jump = split_pos });

        const end_pos = compiler.currentPos();
        compiler.patchSplit(split_pos, body_start, end_pos);
    }
}

/// Helper function to count capture groups in AST
fn countCaptureGroups(node: ASTNode) u32 {
    switch (node) {
        .Group => |g| {
            var count: u32 = 0;
            if (g.index != null) {
                count = 1;
            }
            return count + countCaptureGroups(g.child.*);
        },

        .Concat => |children| {
            var count: u32 = 0;
            for (children) |child| {
                count += countCaptureGroups(child);
            }
            return count;
        },

        .Alternate => |alt| {
            return countCaptureGroups(alt.left.*) + countCaptureGroups(alt.right.*);
        },

        .Quantifier => |q| {
            return countCaptureGroups(q.child.*);
        },

        else => return 0,
    }
}

/// Main compile function - takes AST and produces CompiledRegex
pub fn compile(allocator: std.mem.Allocator, ast: ASTNode) !CompiledRegex {
    var compiler = Compiler.init(allocator);
    errdefer compiler.deinit();

    // Count capture groups
    const capture_groups = countCaptureGroups(ast);

    // Emit entry point that wraps the entire match in group 0
    try compiler.emit(.{ .Save = 0 }); // Save 0 = start of full match

    // Compile the AST
    try compileNode(&compiler, ast);

    // Emit end of group 0 and match success
    try compiler.emit(.{ .Save = 1 }); // Save 1 = end of full match
    try compiler.emit(.Match);

    // Move instructions to heap
    const instructions = try compiler.instructions.toOwnedSlice(allocator);

    return CompiledRegex{
        .instructions = instructions,
        .capture_groups = capture_groups + 1, // +1 for group 0 (full match)
        .allocator = allocator,
    };
}

/// AST manipulation helpers for creating test nodes
/// These would normally be used by the parser

pub fn createLiteral(allocator: std.mem.Allocator, c: u8) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .{ .Literal = c };
    return node;
}

pub fn createAny(allocator: std.mem.Allocator) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .Any;
    return node;
}

pub fn createConcat(allocator: std.mem.Allocator, children: []ASTNode) !*ASTNode {
    const node = try allocator.create(ASTNode);
    const owned_children = try allocator.dupe(ASTNode, children);
    node.* = .{ .Concat = owned_children };
    return node;
}

pub fn createAlternate(allocator: std.mem.Allocator, left: *ASTNode, right: *ASTNode) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .{ .Alternate = .{ .left = left, .right = right } };
    return node;
}

pub fn createQuantifier(
    allocator: std.mem.Allocator,
    child: *ASTNode,
    min: u32,
    max: ?u32,
    greedy: bool,
) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .{ .Quantifier = .{
        .child = child,
        .min = min,
        .max = max,
        .greedy = greedy,
    } };
    return node;
}

pub fn createGroup(allocator: std.mem.Allocator, child: *ASTNode, index: ?u32) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .{ .Group = .{ .child = child, .index = index } };
    return node;
}

pub fn createClass(allocator: std.mem.Allocator, bitmap: CharacterClassBitmap, negated: bool) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .{ .Class = .{ .bitmap = bitmap, .negated = negated } };
    return node;
}

pub fn createAnchorLineStart(allocator: std.mem.Allocator) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .AnchorLineStart;
    return node;
}

pub fn createAnchorLineEnd(allocator: std.mem.Allocator) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .AnchorLineEnd;
    return node;
}

pub fn createWordBoundary(allocator: std.mem.Allocator, is_boundary: bool) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .{ .WordBoundary = is_boundary };
    return node;
}

pub fn createBackref(allocator: std.mem.Allocator, n: u32) !*ASTNode {
    const node = try allocator.create(ASTNode);
    node.* = .{ .Backref = n };
    return node;
}

/// Free an AST node tree
pub fn destroyNode(allocator: std.mem.Allocator, node: *ASTNode) void {
    switch (node.*) {
        .Concat => |children| {
            allocator.free(children);
        },
        .Alternate => |alt| {
            destroyNode(allocator, alt.left);
            destroyNode(allocator, alt.right);
        },
        .Quantifier => |q| {
            destroyNode(allocator, q.child);
        },
        .Group => |g| {
            destroyNode(allocator, g.child);
        },
        else => {},
    }
    allocator.destroy(node);
}

// ============================================
// Tests
// ============================================

const testing = std.testing;

test "compile literal" {
    const allocator = testing.allocator;

    const ast = ASTNode{ .Literal = 'a' };
    var compiled = try compile(allocator, ast);
    defer compiled.deinit();

    try testing.expectEqual(@as(u32, 1), compiled.capture_groups);
    try testing.expectEqual(@as(usize, 4), compiled.instructions.len);
    try testing.expectEqual(@as(u32, 0), compiled.instructions[0].Save);
    try testing.expectEqual(Instruction{ .Char = 'a' }, compiled.instructions[1]);
    try testing.expectEqual(@as(u32, 1), compiled.instructions[2].Save);
    try testing.expectEqual(Instruction.Match, compiled.instructions[3]);
}

test "compile concat" {
    const allocator = testing.allocator;

    const children = try allocator.dupe(ASTNode, &[_]ASTNode{
        .{ .Literal = 'a' },
        .{ .Literal = 'b' },
    });
    defer allocator.free(children);

    const ast = ASTNode{ .Concat = children };
    var compiled = try compile(allocator, ast);
    defer compiled.deinit();

    try testing.expectEqual(@as(usize, 5), compiled.instructions.len);
    try testing.expectEqual(Instruction{ .Char = 'a' }, compiled.instructions[1]);
    try testing.expectEqual(Instruction{ .Char = 'b' }, compiled.instructions[2]);
}

test "compile alternate" {
    const allocator = testing.allocator;

    const left = try createLiteral(allocator, 'a');
    const right = try createLiteral(allocator, 'b');

    const alt = try createAlternate(allocator, left, right);
    defer destroyNode(allocator, alt);

    var compiled = try compile(allocator, alt.*);
    defer compiled.deinit();

    // Should have: Save(0), Split, Char('a'), Jump, Char('b'), Save(1), Match
    try testing.expectEqual(@as(usize, 7), compiled.instructions.len);
    try testing.expectEqual(Instruction.Split, std.meta.activeTag(compiled.instructions[1]));
}

test "compile star quantifier" {
    const allocator = testing.allocator;

    const child = try createLiteral(allocator, 'a');

    const star = try createQuantifier(allocator, child, 0, null, true);
    defer destroyNode(allocator, star);

    var compiled = try compile(allocator, star.*);
    defer compiled.deinit();

    // Should have loop structure
    try testing.expect(compiled.instructions.len >= 5);
    try testing.expectEqual(Instruction.Split, std.meta.activeTag(compiled.instructions[1]));
}

test "compile capturing group" {
    const allocator = testing.allocator;

    const child = try createLiteral(allocator, 'a');

    const group = try createGroup(allocator, child, 1);
    defer destroyNode(allocator, group);

    var compiled = try compile(allocator, group.*);
    defer compiled.deinit();

    try testing.expectEqual(@as(u32, 2), compiled.capture_groups); // 0 + 1
    try testing.expectEqual(Instruction{ .Save = 2 }, compiled.instructions[1]); // Start of group 1
    try testing.expectEqual(Instruction{ .Save = 3 }, compiled.instructions[3]); // End of group 1
}

test "compile character class" {
    const allocator = testing.allocator;

    var bitmap = CharacterClassBitmap.init();
    bitmap.setRange('a', 'z');

    const class_node = try createClass(allocator, bitmap, false);
    defer allocator.destroy(class_node);

    var compiled = try compile(allocator, class_node.*);
    defer compiled.deinit();

    try testing.expectEqual(Instruction.Class, std.meta.activeTag(compiled.instructions[1]));
}

test "compile anchors" {
    const allocator = testing.allocator;

    const start = try createAnchorLineStart(allocator);
    defer allocator.destroy(start);

    var compiled = try compile(allocator, start.*);
    defer compiled.deinit();

    try testing.expectEqual(Instruction.LineStart, std.meta.activeTag(compiled.instructions[1]));
}

test "compile backref" {
    const allocator = testing.allocator;

    const backref = try createBackref(allocator, 1);
    defer allocator.destroy(backref);

    var compiled = try compile(allocator, backref.*);
    defer compiled.deinit();

    try testing.expectEqual(@as(u32, 1), compiled.instructions[1].Backref);
}

test "compile complex regex" {
    const allocator = testing.allocator;

    // Pattern: (a|b)*c
    const a_lit = try createLiteral(allocator, 'a');
    const b_lit = try createLiteral(allocator, 'b');
    const alt = try createAlternate(allocator, a_lit, b_lit);
    const group = try createGroup(allocator, alt, 1);
    const star = try createQuantifier(allocator, group, 0, null, true);
    const c_lit = try createLiteral(allocator, 'c');

    const children = try allocator.dupe(ASTNode, &[_]ASTNode{ star.*, c_lit.* });
    const concat = ASTNode{ .Concat = children };

    var compiled = try compile(allocator, concat);
    defer {
        compiled.deinit();
        allocator.free(children);
        // star owns the entire subtree (group -> alt -> a_lit, b_lit)
        destroyNode(allocator, star);
        allocator.destroy(c_lit);
    }

    // Should compile without errors and have reasonable instruction count
    try testing.expect(compiled.instructions.len > 5);
    try testing.expectEqual(@as(u32, 2), compiled.capture_groups);
}
