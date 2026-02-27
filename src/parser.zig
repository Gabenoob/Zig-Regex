//! Regex Parser Module
//! Tokenizes regex patterns and builds an Abstract Syntax Tree (AST)

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Parser error types
pub const ParserError = error{
    InvalidPattern,
    UnmatchedParenthesis,
    UnmatchedBracket,
    InvalidEscape,
    InvalidGroup,
    InvalidRange,
    InvalidQuantifier,
    OutOfMemory,
};

/// Anchor types for ^, $, and word boundaries
pub const AnchorType = enum {
    StartLine,
    EndLine,
    WordBoundary,
    NotWordBoundary,
};

/// Character range for character classes
pub const CharRange = struct {
    start: u8,
    end: u8,
};

/// AST Node types
pub const Node = union(enum) {
    Literal: u8,
    Any,
    Concat: struct { left: *Node, right: *Node },
    Alternate: struct { left: *Node, right: *Node },
    Quantifier: struct {
        child: *Node,
        min: usize,
        max: usize,
        greedy: bool,
    },
    Group: struct {
        child: *Node,
        index: u32,
        name: ?[]const u8,
    },
    CharacterClass: struct {
        ranges: []CharRange,
        negated: bool,
    },
    Anchor: AnchorType,
    Backreference: u32,

    /// Recursively deallocate the node and its children
    pub fn deinit(self: *Node, allocator: Allocator) void {
        switch (self.*) {
            .Literal, .Any, .Anchor, .Backreference => {},
            .Concat => |c| {
                c.left.deinit(allocator);
                c.right.deinit(allocator);
            },
            .Alternate => |a| {
                a.left.deinit(allocator);
                a.right.deinit(allocator);
            },
            .Quantifier => |q| {
                q.child.deinit(allocator);
            },
            .Group => |g| {
                g.child.deinit(allocator);
                if (g.name) |name| {
                    allocator.free(name);
                }
            },
            .CharacterClass => |cc| {
                allocator.free(cc.ranges);
            },
        }
        allocator.destroy(self);
    }

    /// Format the node for debugging
    pub fn format(self: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Literal => |c| try writer.print("Literal({c})", .{c}),
            .Any => try writer.writeAll("Any"),
            .Concat => |c| {
                try writer.writeAll("Concat(");
                try c.left.format("", .{}, writer);
                try writer.writeAll(", ");
                try c.right.format("", .{}, writer);
                try writer.writeAll(")");
            },
            .Alternate => |a| {
                try writer.writeAll("Alternate(");
                try a.left.format("", .{}, writer);
                try writer.writeAll(", ");
                try a.right.format("", .{}, writer);
                try writer.writeAll(")");
            },
            .Quantifier => |q| {
                try writer.print("Quantifier(min={}, max={}, greedy={}, ", .{ q.min, q.max, q.greedy });
                try q.child.format("", .{}, writer);
                try writer.writeAll(")");
            },
            .Group => |g| {
                try writer.print("Group(index={}, name={s}, ", .{ g.index, if (g.name) |n| n else "null" });
                try g.child.format("", .{}, writer);
                try writer.writeAll(")");
            },
            .CharacterClass => |cc| {
                try writer.print("CharacterClass(negated={}, ranges=[", .{cc.negated});
                for (cc.ranges, 0..) |r, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{c}-{c}", .{ r.start, r.end });
                }
                try writer.writeAll("])");
            },
            .Anchor => |a| try writer.print("Anchor({s})", .{@tagName(a)}),
            .Backreference => |b| try writer.print("Backreference({})", .{b}),
        }
    }
};

/// Token types for the tokenizer
const TokenType = enum {
    Literal,
    Any,
    Star,
    Plus,
    Question,
    OpenBrace,
    CloseBrace,
    Comma,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Pipe,
    Caret,
    Dollar,
    Escape,
    End,
};

const Token = struct {
    type: TokenType,
    char: u8 = 0,
    position: usize,
};

/// Tokenizer for regex patterns
const Tokenizer = struct {
    pattern: []const u8,
    position: usize,

    fn init(pattern: []const u8) Tokenizer {
        return .{
            .pattern = pattern,
            .position = 0,
        };
    }

    fn peek(self: *Tokenizer) ?u8 {
        if (self.position >= self.pattern.len) return null;
        return self.pattern[self.position];
    }

    fn consume(self: *Tokenizer) ?u8 {
        const ch = self.peek() orelse return null;
        self.position += 1;
        return ch;
    }

    fn nextToken(self: *Tokenizer) Token {
        const start_pos = self.position;
        const ch = self.consume() orelse {
            return .{ .type = .End, .position = start_pos };
        };

        switch (ch) {
            '.' => return .{ .type = .Any, .position = start_pos },
            '*' => return .{ .type = .Star, .position = start_pos },
            '+' => return .{ .type = .Plus, .position = start_pos },
            '?' => return .{ .type = .Question, .position = start_pos },
            '{' => return .{ .type = .OpenBrace, .position = start_pos },
            '}' => return .{ .type = .CloseBrace, .position = start_pos },
            ',' => return .{ .type = .Comma, .position = start_pos },
            '(' => return .{ .type = .OpenParen, .position = start_pos },
            ')' => return .{ .type = .CloseParen, .position = start_pos },
            '[' => return .{ .type = .OpenBracket, .position = start_pos },
            ']' => return .{ .type = .CloseBracket, .position = start_pos },
            '|' => return .{ .type = .Pipe, .position = start_pos },
            '^' => return .{ .type = .Caret, .position = start_pos },
            '$' => return .{ .type = .Dollar, .position = start_pos },
            '\\' => {
                const escaped = self.consume();
                if (escaped) |e| {
                    return .{ .type = .Escape, .char = e, .position = start_pos };
                } else {
                    return .{ .type = .Literal, .char = '\\', .position = start_pos };
                }
            },
            else => return .{ .type = .Literal, .char = ch, .position = start_pos },
        }
    }
};

/// Parser state
const Parser = struct {
    tokenizer: Tokenizer,
    current: Token,
    allocator: Allocator,
    group_index: u32,

    fn init(pattern: []const u8, allocator: Allocator) !Parser {
        var tokenizer = Tokenizer.init(pattern);
        const current = tokenizer.nextToken();
        return .{
            .tokenizer = tokenizer,
            .current = current,
            .allocator = allocator,
            .group_index = 1,
        };
    }

    fn advance(self: *Parser) void {
        self.current = self.tokenizer.nextToken();
    }

    fn expect(self: *Parser, token_type: TokenType) !void {
        if (self.current.type != token_type) {
            return ParserError.InvalidPattern;
        }
        self.advance();
    }

    fn parse(self: *Parser) ParserError!*Node {
        const node = try self.parseAlternation();
        if (self.current.type != .End) {
            return ParserError.InvalidPattern;
        }
        return node;
    }

    fn parseAlternation(self: *Parser) ParserError!*Node {
        var left = try self.parseConcatenation();

        while (self.current.type == .Pipe) {
            self.advance();
            const right = try self.parseConcatenation();

            const alt_node = try self.allocator.create(Node);
            alt_node.* = .{ .Alternate = .{ .left = left, .right = right } };
            left = alt_node;
        }

        return left;
    }

    fn parseConcatenation(self: *Parser) ParserError!*Node {
        if (try self.isTermEnd()) {
            const node = try self.allocator.create(Node);
            node.* = .{ .Literal = 0 };
            return node;
        }

        var left = try self.parseTerm();

        while (!try self.isTermEnd()) {
            const right = try self.parseTerm();

            const concat_node = try self.allocator.create(Node);
            concat_node.* = .{ .Concat = .{ .left = left, .right = right } };
            left = concat_node;
        }

        return left;
    }

    fn isTermEnd(self: *Parser) !bool {
        return switch (self.current.type) {
            .End, .CloseParen, .Pipe => true,
            else => false,
        };
    }

    fn parseTerm(self: *Parser) ParserError!*Node {
        const atom = try self.parseAtom();
        return try self.parseQuantifier(atom);
    }

    fn parseQuantifier(self: *Parser, child: *Node) ParserError!*Node {
        var min: usize = 0;
        var max: usize = std.math.maxInt(usize);
        var greedy = true;

        switch (self.current.type) {
            .Star => {
                min = 0;
                max = std.math.maxInt(usize);
                self.advance();
            },
            .Plus => {
                min = 1;
                max = std.math.maxInt(usize);
                self.advance();
            },
            .Question => {
                min = 0;
                max = 1;
                self.advance();
            },
            .OpenBrace => {
                const result = try self.parseBraceQuantifier();
                min = result.min;
                max = result.max;
            },
            else => return child,
        }

        if (self.current.type == .Question) {
            greedy = false;
            self.advance();
        }

        const node = try self.allocator.create(Node);
        node.* = .{ .Quantifier = .{
            .child = child,
            .min = min,
            .max = max,
            .greedy = greedy,
        } };
        return node;
    }

    const QuantifierResult = struct {
        min: usize,
        max: usize,
    };

    fn parseBraceQuantifier(self: *Parser) ParserError!QuantifierResult {
        try self.expect(.OpenBrace);

        const min = try self.parseNumber();
        var max = min;

        if (self.current.type == .CloseBrace) {
            self.advance();
            return .{ .min = min, .max = max };
        }

        if (self.current.type == .Comma) {
            self.advance();
            max = std.math.maxInt(usize);

            if (self.current.type != .CloseBrace) {
                max = try self.parseNumber();
            }
        }

        try self.expect(.CloseBrace);

        if (min > max) {
            return ParserError.InvalidQuantifier;
        }

        return .{ .min = min, .max = max };
    }

    fn parseNumber(self: *Parser) ParserError!usize {
        var num: usize = 0;
        var has_digits = false;

        while (self.current.type == .Literal and std.ascii.isDigit(self.current.char)) {
            has_digits = true;
            const digit = self.current.char - '0';
            num = num * 10 + digit;
            self.advance();
        }

        if (!has_digits) {
            return ParserError.InvalidQuantifier;
        }

        return num;
    }

    fn parseAtom(self: *Parser) ParserError!*Node {
        switch (self.current.type) {
            .Literal => {
                const ch = self.current.char;
                self.advance();
                const node = try self.allocator.create(Node);
                node.* = .{ .Literal = ch };
                return node;
            },
            .Comma => {
                self.advance();
                const node = try self.allocator.create(Node);
                node.* = .{ .Literal = ',' };
                return node;
            },
            .Escape => {
                return try self.parseEscape();
            },
            .Any => {
                self.advance();
                const node = try self.allocator.create(Node);
                node.* = .Any;
                return node;
            },
            .OpenBracket => {
                return try self.parseCharacterClass();
            },
            .OpenParen => {
                return try self.parseGroup();
            },
            .Caret => {
                self.advance();
                const node = try self.allocator.create(Node);
                node.* = .{ .Anchor = .StartLine };
                return node;
            },
            .Dollar => {
                self.advance();
                const node = try self.allocator.create(Node);
                node.* = .{ .Anchor = .EndLine };
                return node;
            },
            else => {
                return ParserError.InvalidPattern;
            },
        }
    }

    fn parseEscape(self: *Parser) ParserError!*Node {
        const ch = self.current.char;
        self.advance();

        const node = try self.allocator.create(Node);

        switch (ch) {
            'd' => {
                const ranges = try self.allocator.alloc(CharRange, 1);
                ranges[0] = .{ .start = '0', .end = '9' };
                node.* = .{ .CharacterClass = .{ .ranges = ranges, .negated = false } };
                return node;
            },
            'D' => {
                const ranges = try self.allocator.alloc(CharRange, 1);
                ranges[0] = .{ .start = '0', .end = '9' };
                node.* = .{ .CharacterClass = .{ .ranges = ranges, .negated = true } };
                return node;
            },
            'w' => {
                const ranges = try self.allocator.alloc(CharRange, 4);
                ranges[0] = .{ .start = 'a', .end = 'z' };
                ranges[1] = .{ .start = 'A', .end = 'Z' };
                ranges[2] = .{ .start = '0', .end = '9' };
                ranges[3] = .{ .start = '_', .end = '_' };
                node.* = .{ .CharacterClass = .{ .ranges = ranges, .negated = false } };
                return node;
            },
            'W' => {
                const ranges = try self.allocator.alloc(CharRange, 4);
                ranges[0] = .{ .start = 'a', .end = 'z' };
                ranges[1] = .{ .start = 'A', .end = 'Z' };
                ranges[2] = .{ .start = '0', .end = '9' };
                ranges[3] = .{ .start = '_', .end = '_' };
                node.* = .{ .CharacterClass = .{ .ranges = ranges, .negated = true } };
                return node;
            },
            's' => {
                const ranges = try self.allocator.alloc(CharRange, 5);
                ranges[0] = .{ .start = ' ', .end = ' ' };
                ranges[1] = .{ .start = '\t', .end = '\t' };
                ranges[2] = .{ .start = '\n', .end = '\n' };
                ranges[3] = .{ .start = '\r', .end = '\r' };
                ranges[4] = .{ .start = '\x0c', .end = '\x0c' };
                node.* = .{ .CharacterClass = .{ .ranges = ranges, .negated = false } };
                return node;
            },
            'S' => {
                const ranges = try self.allocator.alloc(CharRange, 5);
                ranges[0] = .{ .start = ' ', .end = ' ' };
                ranges[1] = .{ .start = '\t', .end = '\t' };
                ranges[2] = .{ .start = '\n', .end = '\n' };
                ranges[3] = .{ .start = '\r', .end = '\r' };
                ranges[4] = .{ .start = '\x0c', .end = '\x0c' };
                node.* = .{ .CharacterClass = .{ .ranges = ranges, .negated = true } };
                return node;
            },
            'b' => {
                node.* = .{ .Anchor = .WordBoundary };
                return node;
            },
            'B' => {
                node.* = .{ .Anchor = .NotWordBoundary };
                return node;
            },
            '0'...'9' => {
                var index: u32 = ch - '0';
                while (self.current.type == .Literal and std.ascii.isDigit(self.current.char)) {
                    index = index * 10 + (self.current.char - '0');
                    self.advance();
                }
                node.* = .{ .Backreference = index };
                return node;
            },
            't' => {
                node.* = .{ .Literal = '\t' };
                return node;
            },
            'n' => {
                node.* = .{ .Literal = '\n' };
                return node;
            },
            'r' => {
                node.* = .{ .Literal = '\r' };
                return node;
            },
            '\\', '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '^', '$', '|' => {
                node.* = .{ .Literal = ch };
                return node;
            },
            else => {
                node.* = .{ .Literal = ch };
                return node;
            },
        }
    }

    fn parseCharacterClass(self: *Parser) ParserError!*Node {
        try self.expect(.OpenBracket);

        var negated = false;
        if (self.current.type == .Caret) {
            negated = true;
            self.advance();
        }

        var ranges: std.ArrayList(CharRange) = .empty;
        defer ranges.deinit(self.allocator);

        var last_char: ?u8 = null;

        while (self.current.type != .CloseBracket and self.current.type != .End) {
            if (self.current.type == .Escape) {
                const escaped = self.current.char;
                self.advance();

                const ch: u8 = switch (escaped) {
                    't' => '\t',
                    'n' => '\n',
                    'r' => '\r',
                    '\\', ']', '-', '^' => escaped,
                    else => escaped,
                };

                if (last_char != null and self.current.type == .Literal and self.current.char == '-') {
                    self.advance();
                    if (self.current.type == .End) break;

                    const end_ch = if (self.current.type == .Escape) blk: {
                        const e = self.current.char;
                        self.advance();
                        break :blk switch (e) {
                            't' => '\t',
                            'n' => '\n',
                            'r' => '\r',
                            else => e,
                        };
                    } else if (self.current.type == .Literal) blk2: {
                        const l = self.current.char;
                        self.advance();
                        break :blk2 l;
                    } else break;

                    try ranges.append(self.allocator, .{ .start = last_char.?, .end = end_ch });
                    last_char = null;
                } else {
                    if (last_char) |lc| {
                        try ranges.append(self.allocator, .{ .start = lc, .end = lc });
                    }
                    last_char = ch;
                }
            } else if (self.current.type == .Literal) {
                const ch = self.current.char;
                self.advance();

                if (last_char != null and ch == '-') {
                    if (self.current.type == .End) {
                        try ranges.append(self.allocator, .{ .start = '-', .end = '-' });
                        last_char = null;
                    } else {
                        const start = last_char.?;

                        const end_ch = if (self.current.type == .Escape) blk: {
                            const e = self.current.char;
                            self.advance();
                            break :blk switch (e) {
                                't' => '\t',
                                'n' => '\n',
                                'r' => '\r',
                                else => e,
                            };
                        } else if (self.current.type == .Literal) blk2: {
                            const l = self.current.char;
                            self.advance();
                            break :blk2 l;
                        } else break;

                        try ranges.append(self.allocator, .{ .start = start, .end = end_ch });
                        last_char = null;
                    }
                } else {
                    if (last_char) |lc| {
                        try ranges.append(self.allocator, .{ .start = lc, .end = lc });
                    }
                    last_char = ch;
                }
            } else {
                self.advance();
            }
        }

        if (last_char) |lc| {
            try ranges.append(self.allocator, .{ .start = lc, .end = lc });
        }

        try self.expect(.CloseBracket);

        const node = try self.allocator.create(Node);
        node.* = .{ .CharacterClass = .{
            .ranges = try ranges.toOwnedSlice(self.allocator),
            .negated = negated,
        } };
        return node;
    }

    fn parseGroup(self: *Parser) ParserError!*Node {
        try self.expect(.OpenParen);

        var name: ?[]const u8 = null;

        if (self.current.type == .Question) {
            self.advance();

            if (self.current.type == .Literal) {
                const modifier = self.current.char;
                self.advance();

                switch (modifier) {
                    ':' => {
                        const child = try self.parseAlternation();
                        try self.expect(.CloseParen);

                        const node = try self.allocator.create(Node);
                        node.* = .{ .Group = .{ .child = child, .index = 0, .name = null } };
                        return node;
                    },
                    'P' => {
                        if (self.current.type == .Literal and self.current.char == '<') {
                            self.advance();

                            var name_list: std.ArrayList(u8) = .empty;
                            defer name_list.deinit(self.allocator);

                            while (self.current.type != .CloseParen and self.current.type != .End and
                                !(self.current.type == .Literal and self.current.char == '>'))
                            {
                                if (self.current.type == .Literal or self.current.type == .Escape) {
                                    try name_list.append(self.allocator, self.current.char);
                                    self.advance();
                                } else {
                                    break;
                                }
                            }

                            if (self.current.type == .Literal and self.current.char == '>') {
                                self.advance();
                            }

                            name = try name_list.toOwnedSlice(self.allocator);
                        } else {
                            return ParserError.InvalidGroup;
                        }
                    },
                    else => {
                        return ParserError.InvalidGroup;
                    },
                }
            }
        }

        const group_idx = self.group_index;
        self.group_index += 1;

        const child = try self.parseAlternation();
        try self.expect(.CloseParen);

        const node = try self.allocator.create(Node);
        node.* = .{ .Group = .{ .child = child, .index = group_idx, .name = name } };
        return node;
    }
};

/// Parse a regex pattern and return the AST root node
pub fn parse(pattern: []const u8, allocator: Allocator) ParserError!*Node {
    var p = try Parser.init(pattern, allocator);
    return try p.parse();
}

/// Get the number of capture groups in a pattern
pub fn countGroups(pattern: []const u8) ParserError!u32 {
    var count: u32 = 0;
    var in_escape = false;
    var i: usize = 0;

    while (i < pattern.len) : (i += 1) {
        const ch = pattern[i];

        if (in_escape) {
            in_escape = false;
            continue;
        }

        if (ch == '\\') {
            in_escape = true;
        } else if (ch == '(') {
            if (i + 2 < pattern.len and pattern[i + 1] == '?' and pattern[i + 2] == ':') {
                i += 2;
            } else if (i + 2 < pattern.len and pattern[i + 1] == '?' and pattern[i + 2] == 'P') {
                i += 2;
                count += 1;
            } else {
                count += 1;
            }
        }
    }

    return count;
}

// Tests
const testing = std.testing;

test "parse literal" {
    const allocator = testing.allocator;
    const ast = try parse("abc", allocator);
    defer ast.deinit(allocator);
    try testing.expect(ast.* == .Concat);
}

test "parse quantifiers" {
    const allocator = testing.allocator;
    const ast = try parse("a*", allocator);
    defer ast.deinit(allocator);
    try testing.expect(ast.* == .Quantifier);
    try testing.expectEqual(@as(usize, 0), ast.Quantifier.min);
}

test "parse alternation" {
    const allocator = testing.allocator;
    const ast = try parse("a|b", allocator);
    defer ast.deinit(allocator);
    try testing.expect(ast.* == .Alternate);
}

test "parse character class" {
    const allocator = testing.allocator;
    const ast = try parse("[abc]", allocator);
    defer ast.deinit(allocator);
    try testing.expect(ast.* == .CharacterClass);
}

test "parse groups" {
    const allocator = testing.allocator;
    const ast = try parse("(ab)", allocator);
    defer ast.deinit(allocator);
    try testing.expect(ast.* == .Group);
    try testing.expectEqual(@as(u32, 1), ast.Group.index);
}

test "count groups" {
    try testing.expectEqual(@as(u32, 0), try countGroups("abc"));
    try testing.expectEqual(@as(u32, 1), try countGroups("(abc)"));
    try testing.expectEqual(@as(u32, 2), try countGroups("(a)(b)"));
}
