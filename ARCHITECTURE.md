# Zig Regex Module - Project Architecture

## Overview
A Zig regular expression module inspired by Python's `re` module.

## API Interface (Python re-inspired)

### Core Functions
```zig
// Compile pattern
pub fn compile(pattern: []const u8) RegexError!Regex
pub fn compileWithFlags(pattern: []const u8, flags: RegexFlags) RegexError!Regex

// Match operations
pub fn match(regex: *const Regex, text: []const u8) RegexError!?Match
pub fn search(regex: *const Regex, text: []const u8) RegexError!?Match
pub fn findAll(regex: *const Regex, text: []const u8, allocator: std.mem.Allocator) RegexError![]Match

// Replace operations
pub fn sub(regex: *const Regex, replacement: []const u8, text: []const u8, allocator: std.mem.Allocator) RegexError![]u8
pub fn subn(regex: *const Regex, replacement: []const u8, text: []const u8, allocator: std.mem.Allocator) RegexError!SubResult

// Split operation
pub fn split(regex: *const Regex, text: []const u8, allocator: std.mem.Allocator) RegexError![][]u8

// Escape utility
pub fn escape(text: []const u8, allocator: std.mem.Allocator) RegexError![]u8
```

### Types
```zig
pub const Regex = struct {
    pattern: []const u8,
    flags: RegexFlags,
    program: []Instruction,  // Compiled bytecode
    capture_groups: u32,
    
    // Methods
    pub fn deinit(self: *Regex, allocator: std.mem.Allocator) void;
    pub fn match(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn search(self: *const Regex, text: []const u8) RegexError!?Match;
    pub fn findAll(self: *const Regex, text: []const u8, allocator: std.mem.Allocator) RegexError![]Match;
    pub fn sub(self: *const Regex, replacement: []const u8, text: []const u8, allocator: std.mem.Allocator) RegexError![]u8;
    pub fn subn(self: *const Regex, replacement: []const u8, text: []const u8, allocator: std.mem.Allocator) RegexError!SubResult;
    pub fn split(self: *const Regex, text: []const u8, allocator: std.mem.Allocator) RegexError![][]u8;
};

pub const Match = struct {
    text: []const u8,           // Original text
    start: usize,               // Match start position
    end: usize,                 // Match end position
    groups: []?Group,           // Capture groups (0 = full match)
    
    pub fn get(self: *const Match, group_index: usize) ?[]const u8;
    pub fn getNamed(self: *const Match, name: []const u8) ?[]const u8;
    pub fn span(self: *const Match) struct { usize, usize };
    pub fn groupsSlice(self: *const Match) []const ?[]const u8;
};

pub const Group = struct {
    start: usize,
    end: usize,
    name: ?[]const u8,
};

pub const SubResult = struct {
    result: []u8,
    count: usize,
};

pub const RegexFlags = packed struct {
    ignore_case: bool = false,      // i flag
    multiline: bool = false,        // m flag (^/$ match line boundaries)
    dotall: bool = false,           // s flag (. matches newlines)
    verbose: bool = false,          // x flag (whitespace ignored)
    unicode: bool = true,           // u flag (unicode support)
};

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

## Module Architecture

### 1. Parser Module (`parser.zig`)
- Tokenizes regex patterns
- Builds Abstract Syntax Tree (AST)
- Handles: literals, quantifiers, groups, character classes, anchors

### 2. Compiler Module (`compiler.zig`)
- Compiles AST to bytecode instructions
- Instruction set: Char, Match, Split, Jump, Save, Any, Range, etc.
- Optimizations: instruction fusion, peephole optimization

### 3. VM/Engine Module (`vm.zig`)
- Executes compiled bytecode
- Backtracking NFA simulation
- Thread management for parallel path exploration

### 4. Match Module (`match.zig`)
- Match result handling
- Group extraction
- Named group support

### 5. Main API Module (`regex.zig`)
- Public API surface
- Convenience functions
- Error handling

### 6. Utils Module (`utils.zig`)
- Common utilities
- Escape helpers
- String operations

## Bytecode Instructions
```
Char(c)       - Match literal character c
Any           - Match any character (except newline unless dotall)
Range(a,b)    - Match character in range [a-b]
Class(bitmap) - Match character in character class
Split(x,y)    - Try both paths (non-deterministic)
Jump(x)       - Jump to instruction x
Save(n)       - Save position to capture group n
Match         - Successful match
LineStart     - Match ^ (start of string/line)
LineEnd       - Match $ (end of string/line)
WordBoundary  - Match word boundary
Backref(n)    - Match previously captured group n
```

## Directory Structure
```
regex/
├── regex.zig       # Main public API
├── parser.zig      # Pattern parser
├── compiler.zig    # Bytecode compiler
├── vm.zig          # Virtual machine
├── match.zig       # Match result types
├── utils.zig       # Utilities
└── tests/
    ├── test_parser.zig
    ├── test_compiler.zig
    ├── test_vm.zig
    └── test_integration.zig
```
