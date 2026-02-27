# Zig Regex

A regular expression library for Zig, inspired by Python's `re` module. This library provides a complete regex implementation with pattern matching, capture groups, and text manipulation operations.

## Features

- **Pattern Matching**: `match()` and `search()` operations for finding patterns
- **Find All**: Locate all occurrences of a pattern in text
- **Capture Groups**: Extract and access matched groups
- **Text Replacement**: `sub()` and `subn()` for pattern-based replacements
- **Text Splitting**: Split strings by regex patterns
- **Flags Support**: `ignore_case`, `multiline`, `dotall`, `verbose`, and `unicode` flags
- **Character Classes**: Full support for `[...]`, `\d`, `\w`, `\s`, etc.
- **Quantifiers**: `*`, `+`, `?`, `{n,m}` and greedy/non-greedy variants
- **Anchors**: `^` and `$` for position matching
- **Alternation**: `|` operator for multiple patterns

## Installation

Run the following command in your project:

```bash
zig fetch --save "https://github.com/Gabenoob/Zig-Regex/archive/refs/tags/0.1.1.tar.gz"
```

Then in `build.zig`, add the dependency to your executable or library:

```zig
const regex_mod = b.dependency("Zig-Regex", .{}).module("regex");

exe.root_module.addImport("re", regex_mod);
```

## Quick Start

```zig
const std = @import("std");
const re = @import("re");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Compile a pattern
    var re = try re.Regex.compile(allocator, "hello");
    defer re.deinit();

    // Match from the beginning
    if (try re.match("hello world")) |*m| {
        std.debug.print("Matched: {s}\n", .{m.fullMatch()});
        m.deinit();
    }

    // Search anywhere in text
    if (try re.search("say hello there")) |*m| {
        std.debug.print("Found: {s}\n", .{m.fullMatch()});
        m.deinit();
    }
}
```

## API Reference

### Core Types

#### `Regex`
Main regex compilation and matching object.

```zig
pub const Regex = struct {
    pub fn compile(allocator: std.mem.Allocator, pattern: []const u8) RegexError!Regex;
    pub fn compileWithFlags(allocator: std.mem.Allocator, pattern: []const u8, flags: RegexFlags) RegexError!Regex;
    pub fn deinit(self: *Regex) void;
    pub fn match(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn search(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn findAll(self: *const Regex, text: []const u8) RegexError![]Match;
    pub fn sub(self: *const Regex, replacement: []const u8, text: []const u8) RegexError![]u8;
    pub fn subn(self: *const Regex, replacement: []const u8, text: []const u8) RegexError!SubResult;
    pub fn split(self: *const Regex, text: []const u8) RegexError![][]u8;
};
```

#### `Match`
Represents a successful match with captured groups.

```zig
pub const Match = struct {
    text: []const u8;
    start: usize;
    end: usize;
    groups: []?[]const u8;
    
    pub fn fullMatch(self: *const Match) []const u8;
    pub fn get(self: *const Match, index: usize) ?[]const u8;
    pub fn span(self: *const Match) struct { usize, usize };
    pub fn deinit(self: *Match) void;
};
```

#### `RegexFlags`
Compile-time flags for regex behavior.

```zig
pub const RegexFlags = packed struct {
    ignore_case: bool = false,  // Case-insensitive matching
    multiline: bool = false,    // ^ and $ match line boundaries
    dotall: bool = false,       // . matches newlines
    verbose: bool = false,      // Ignore whitespace in pattern
    unicode: bool = true,       // Unicode support
};
```

### Operations

#### Match from start
```zig
if (try re.match("hello world")) |*m| {
    // Pattern matched from the beginning
    m.deinit();
}
```

#### Search anywhere
```zig
if (try re.search("prefix hello suffix")) |*m| {
    // Pattern found anywhere in text
    m.deinit();
}
```

#### Find all matches
```zig
const matches = try re.findAll("hello world hello");
defer {
    for (matches) |*m| m.deinit();
    allocator.free(matches);
}
for (matches) |*m| {
    std.debug.print("{s} ", .{m.fullMatch()});
}
```

#### Replace first occurrence
```zig
const result = try re.sub("goodbye", "hello world");
defer allocator.free(result);
// Result: "goodbye world"
```

#### Replace with count
```zig
const sub_result = try re.subn("!", "hello", "hello hello hello");
defer allocator.free(sub_result.result);
// sub_result.result: "!hello!hello!"
// sub_result.count: 2
```

#### Split by pattern
```zig
const parts = try re.split("one,two,three");
defer {
    for (parts) |p| allocator.free(p);
    allocator.free(parts);
}
// parts: ["one", "two", "three"]
```

## Examples

### Capture Groups
```zig
var re = try regex.Regex.compile(allocator, "(\\w+)=(\\d+)");
defer re.deinit();

if (try re.match("age=25")) |*m| {
    if (m.get(1)) |key| std.debug.print("Key: {s}\n", .{key});     // "age"
    if (m.get(2)) |value| std.debug.print("Value: {s}\n", .{value}); // "25"
    m.deinit();
}
```

### Case-Insensitive Matching
```zig
var flags = regex.RegexFlags{ .ignore_case = true };
var re = try regex.Regex.compileWithFlags(allocator, "HELLO", flags);
defer re.deinit();

if (try re.match("hello")) |*m| {
    // Matches!
    m.deinit();
}
```

### Character Classes
```zig
// Digits
var re = try regex.Regex.compile(allocator, "\\d+");

// Word characters
var re = try regex.Regex.compile(allocator, "\\w+");

// Whitespace
var re = try regex.Regex.compile(allocator, "\\s+");

// Custom class
var re = try regex.Regex.compile(allocator, "[aeiou]");
```

## Building & Testing

### Run Examples
```bash
zig build run
```

### Run Tests
```bash
zig build test
```

### Build for Release
```bash
zig build -Doptimize=ReleaseFast
```

## Architecture

The library is structured in modular components:

- **Parser** (`src/parser.zig`): Tokenizes regex patterns and builds an Abstract Syntax Tree
- **Compiler** (`src/compiler.zig`): Compiles AST to bytecode instructions with optimizations
- **VM** (`src/vm.zig`): Virtual machine that executes compiled regex programs
- **Match** (`src/match.zig`): Match result handling and group extraction
- **Regex** (`src/regex.zig`): High-level API combining all components

See [ARCHITECTURE.md](ARCHITECTURE.md) for detailed design documentation.

## Supported Patterns

| Pattern | Description |
|---------|-------------|
| `.` | Any character (except newline, unless `dotall` flag) |
| `*` | Zero or more of preceding element |
| `+` | One or more of preceding element |
| `?` | Zero or one of preceding element |
| `{n,m}` | Between n and m occurrences |
| `^` | Start of string (or line if `multiline`) |
| `$` | End of string (or line if `multiline`) |
| `\|` | Alternation (or) |
| `[...]` | Character class |
| `[^...]` | Negated character class |
| `(...)` | Capture group |
| `\d` | Digit [0-9] |
| `\w` | Word character [a-zA-Z0-9_] |
| `\s` | Whitespace character |
| `\x` | Escape sequences |

## Error Handling

The library provides detailed error types:

```zig
pub const RegexError = error{
    InvalidPattern,
    UnmatchedParenthesis,
    UnmatchedBracket,
    InvalidEscape,
    InvalidGroup,
    InvalidRange,
    OutOfMemory,
};
```
