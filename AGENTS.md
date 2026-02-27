# Zig Regex - Agent Documentation

## Project Overview

Zig Regex is a regular expression library for Zig, inspired by Python's `re` module. It provides a complete regex implementation with pattern matching, capture groups, and text manipulation operations.

- **Package Name**: `re`
- **Version**: 0.1.0
- **Minimum Zig Version**: 0.15.2
- **Repository**: https://github.com/Gabenoob/Zig-Regex

## Technology Stack

- **Language**: Zig
- **Build System**: Zig's native build system (`build.zig`)
- **Package Manager**: Zig Package Manager (`build.zig.zon`)

## Architecture

The library follows a multi-stage pipeline architecture:

```
Pattern String -> Parser -> AST -> Compiler -> Bytecode -> VM -> Match Result
```

### Module Structure

| Module | File | Purpose |
|--------|------|---------|
| **Public API** | `regex.zig` (root) | Re-exports all public types and functions |
| **Regex** | `src/regex.zig` | High-level `Regex` struct, combines all components |
| **Parser** | `src/parser.zig` | Tokenizes patterns and builds an Abstract Syntax Tree (AST) |
| **Compiler** | `src/compiler.zig` | Compiles AST to bytecode instructions |
| **VM** | `src/vm.zig` | Virtual machine that executes compiled regex programs using NFA simulation |
| **Match** | `src/match.zig` | Match result types, group extraction, capture handling |

### Bytecode Instruction Set

The compiler generates bytecode with these instruction types:
- `Char(c)` - Match literal character c
- `Any` - Match any character
- `Range(a,b)` - Match character in range [a-b]
- `Class(bitmap)` - Match character in character class
- `Split(x,y)` - Try both paths (non-deterministic choice)
- `Jump(x)` - Jump to instruction x
- `Save(n)` - Save position to capture group n
- `Match` - Successful match
- `LineStart` - Match ^ anchor
- `LineEnd` - Match $ anchor
- `WordBoundary` - Match word boundary
- `Backref(n)` - Match previously captured group n

## Directory Structure

```
.
├── build.zig           # Build configuration
├── build.zig.zon       # Package manifest
├── regex.zig           # Public API entry point
├── test.zig            # Test runner entry point
├── README.md           # User documentation
├── ARCHITECTURE.md     # Architecture documentation
├── src/
│   ├── regex.zig       # High-level Regex API
│   ├── parser.zig      # Pattern parser
│   ├── compiler.zig    # Bytecode compiler
│   ├── vm.zig          # Virtual machine (NFA executor)
│   └── match.zig       # Match types and utilities
└── examples/
    └── basic.zig       # Usage examples
```

## Build and Test Commands

```bash
# Run the example program
zig build run

# Run all tests
zig build test

# Build for release
zig build -Doptimize=ReleaseFast

# Clean build artifacts
rm -rf zig-out/ .zig-cache/
```

## Testing Strategy

- Tests are embedded in source files using Zig's `test` blocks
- Each module has its own inline tests
- `test.zig` serves as the test runner, referencing all modules via `std.testing.refAllDeclsRecursive`
- Tests cover unit testing (parser, compiler, VM) and integration scenarios

## Code Style Guidelines

### Naming Conventions
- **Types**: `PascalCase` (e.g., `Regex`, `MatchResult`, `RegexFlags`)
- **Functions**: `camelCase` (e.g., `compile`, `match`, `findAll`)
- **Constants**: `snake_case` or `PascalCase` for types
- **Files**: `snake_case.zig`

### Memory Management
- All regex operations require an allocator parameter
- `Regex.compile()` takes ownership of the pattern string (duplicates it)
- `Match` results contain allocated data and must be freed with `match.deinit()`
- `Regex.deinit()` frees the compiled pattern and internal resources
- ArrayList uses `.empty` initialization pattern with explicit allocator on append

### Error Handling
- Uses Zig's error union type system
- `RegexError` is the main error set containing:
  - `InvalidPattern`
  - `UnmatchedParenthesis`
  - `UnmatchedBracket`
  - `InvalidEscape`
  - `InvalidGroup`
  - `InvalidRange`
  - `InvalidQuantifier`
  - `InvalidBackreference`
  - `OutOfMemory`

### Documentation
- Public API items have doc comments (`///`)
- Module-level documentation uses `//!`

## Public API

### Core Types

```zig
pub const Regex = struct {
    pub fn compile(allocator: std.mem.Allocator, pattern: []const u8) RegexError!Regex;
    pub fn compileWithFlags(allocator: std.mem.Allocator, pattern: []const u8, flags: RegexFlags) RegexError!Regex;
    pub fn deinit(self: *Regex) void;
    
    // Matching operations
    pub fn match(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn search(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn findAll(self: *const Regex, text: []const u8) RegexError![]Match;
    
    // Text manipulation
    pub fn sub(self: *const Regex, replacement: []const u8, text: []const u8) RegexError![]u8;
    pub fn subAll(self: *const Regex, replacement: []const u8, text: []const u8) RegexError![]u8;
    pub fn split(self: *const Regex, text: []const u8) RegexError![][]u8;
};

pub const Match = struct {
    pub fn fullMatch(self: *const Match) []const u8;
    pub fn get(self: *const Match, group_index: usize) ?[]const u8;
    pub fn getNamed(self: *const Match, name: []const u8) ?[]const u8;
    pub fn span(self: *const Match) struct { usize, usize };
    pub fn deinit(self: *const Match) void;
};

pub const RegexFlags = packed struct {
    ignore_case: bool = false,
    multiline: bool = false,
    dotall: bool = false,
    verbose: bool = false,
    unicode: bool = true,
};
```

### Usage Pattern

```zig
const std = @import("std");
const re = @import("re");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Compile pattern
    var regex = try re.Regex.compile(allocator, "hello (\\w+)");
    defer regex.deinit();

    // Match
    if (try regex.match("hello world")) |*m| {
        defer m.deinit();
        std.debug.print("Match: {s}\n", .{m.fullMatch()});
        if (m.get(1)) |group| {
            std.debug.print("Group 1: {s}\n", .{group});
        }
    }
}
```

## Supported Regex Features

- **Literals**: Plain text matching
- **Character Classes**: `[abc]`, `[^abc]`, `[a-z]`, `\d`, `\w`, `\s` (and negated versions)
- **Quantifiers**: `*`, `+`, `?`, `{n,m}`, greedy/non-greedy
- **Anchors**: `^` (start), `$` (end), `\b` (word boundary)
- **Groups**: Capturing `()`, non-capturing `(?:...)`, named `(?P<name>...)`
- **Alternation**: `|` (OR operator)
- **Backreferences**: `\1`, `\2`, etc.
- **Escapes**: `\n`, `\t`, `\r`, `\\`, etc.

## Development Notes

- The parser produces a binary tree AST that gets converted to a flat array form for the compiler
- The VM uses a thread-based NFA simulation with backtracking
- Capture groups use Save instructions with even/odd index pairs (start/end)
- Group 0 is always the full match
- The VM implements leftmost-longest match semantics
