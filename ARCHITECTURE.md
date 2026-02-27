# Zig Regex - Project Architecture

## Overview

Zig Regex is a regular expression library for Zig, inspired by Python's `re` module. It provides a complete regex implementation with pattern matching, capture groups, and text manipulation operations.

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

## Public API

### Core Types

```zig
pub const Regex = struct {
    /// Compile a regex pattern with default flags
    pub fn compile(allocator: std.mem.Allocator, pattern: []const u8) RegexError!Regex;
    
    /// Compile with specific flags
    pub fn compileWithFlags(allocator: std.mem.Allocator, pattern: []const u8, flags: RegexFlags) RegexError!Regex;
    
    /// Free all resources
    pub fn deinit(self: *Regex) void;
    
    // Matching operations
    pub fn match(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn search(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn findAll(self: *const Regex, text: []const u8) RegexError![]Match;
    
    // Text manipulation
    pub fn sub(self: *const Regex, replacement: []const u8, text: []const u8) RegexError![]u8;
    pub fn subAll(self: *const Regex, replacement: []const u8, text: []const u8) RegexError![]u8;
    pub fn split(self: *const Regex, text: []const u8) RegexError![][]u8;
    
    /// Returns the number of capture groups (including group 0)
    pub fn groupCount(self: Regex) usize;
};

pub const Match = struct {
    /// Get the matched text for a capture group by index
    pub fn get(self: *const Match, group_index: usize) ?[]const u8;
    
    /// Get the matched text for a named capture group
    pub fn getNamed(self: *const Match, name: []const u8) ?[]const u8;
    
    /// Get the span (start, end) of the full match
    pub fn span(self: *const Match) struct { usize, usize };
    
    /// Get a span for a specific group by index
    pub fn groupSpan(self: *const Match, group_index: usize) ?struct { usize, usize };
    
    /// Returns the full matched text
    pub fn fullMatch(self: *const Match) []const u8;
    
    /// Returns the length of the match
    pub fn len(self: *const Match) usize;
    
    /// Returns the number of capture groups
    pub fn groupCount(self: *const Match) usize;
    
    /// Release all resources
    pub fn deinit(self: *const Match) void;
};

pub const RegexFlags = packed struct {
    ignore_case: bool = false,
    multiline: bool = false,
    dotall: bool = false,
    verbose: bool = false,
    unicode: bool = true,
};

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
```

### Utility Functions

```zig
/// Escape special regex characters in a string
pub fn escape(allocator: std.mem.Allocator, text: []const u8) RegexError![]u8;
```

## Bytecode Instruction Set

The compiler generates bytecode with these instruction types:

| Instruction | Description |
|-------------|-------------|
| `Char(c)` | Match literal character c |
| `Any` | Match any character |
| `Range(a,b)` | Match character in range [a-b] |
| `Class(bitmap)` | Match character in character class |
| `Split(x,y)` | Try both paths (non-deterministic choice) |
| `Jump(x)` | Jump to instruction x |
| `Save(n)` | Save position to capture group n |
| `Match` | Successful match |
| `LineStart` | Match ^ anchor |
| `LineEnd` | Match $ anchor |
| `WordBoundary` | Match word boundary |
| `Backref(n)` | Match previously captured group n |

## Directory Structure

```
.
├── build.zig           # Build configuration
├── build.zig.zon       # Package manifest
├── regex.zig           # Public API entry point
├── test.zig            # Test runner entry point
├── README.md           # User documentation
├── ARCHITECTURE.md     # This file
├── src/
│   ├── regex.zig       # High-level Regex API
│   ├── parser.zig      # Pattern parser with arena allocation
│   ├── compiler.zig    # Bytecode compiler
│   ├── vm.zig          # Virtual machine (NFA executor)
│   ├── match.zig       # Match types and utilities
│   └── regex_tests.zig # Comprehensive test suite
└── examples/
    └── basic.zig       # Usage examples
```

## Implementation Details

### Parser

The parser tokenizes regex patterns and builds an Abstract Syntax Tree (AST). Key features:

- **Arena Allocation**: Uses an internal arena allocator for parsing, then clones the result to prevent memory leaks on error paths
- **Error Recovery**: Returns specific error types (`UnmatchedParenthesis`, `UnmatchedBracket`, etc.)
- **Supported Syntax**: Literals, quantifiers (`*`, `+`, `?`, `{n,m}`), groups (capturing, non-capturing, named), character classes, anchors, backreferences

### Compiler

Converts the AST to bytecode instructions:

- Flattens the tree structure into a linear instruction sequence
- Handles nested structures via `Split` and `Jump` instructions
- Supports capture groups with `Save` instructions

### VM

Executes compiled bytecode using a backtracking NFA simulation:

- Thread-based execution model
- Leftmost-longest match semantics
- Handles capture groups and backreferences

### Memory Management

All regex operations require an allocator parameter:

- `Regex.compile()` duplicates the pattern string internally
- `Match` results contain allocated data and must be freed with `match.deinit()`
- `Regex.deinit()` frees the compiled pattern and internal resources
- ArrayList uses `.empty` initialization pattern with explicit allocator on operations

## Testing

The library includes a comprehensive test suite (`src/regex_tests.zig`) covering:

- Basic literal matching
- Character classes (`[abc]`, `[^abc]`, `[a-z]`, `\d`, `\w`, `\s`, etc.)
- Quantifiers (`*`, `+`, `?`, `{n,m}`)
- Anchors (`^`, `$`, `\b`)
- Groups (capturing, non-capturing, nested)
- Alternation (`|`)
- Backreferences (`\1`, `\2`)
- Search, findAll, sub, subAll, split operations
- Real-world patterns (email, dates, etc.)
- Edge cases and error handling

Run tests with: `zig build test`
