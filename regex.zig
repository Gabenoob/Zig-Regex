//! Zig Regex - A regular expression library for Zig
//!
//! This library provides regex pattern matching inspired by Python's `re` module.
//!
//! ## Basic Usage
//!
//! ```zig
//! const std = @import("std");
//! const re = @import("re");
//!
//! pub fn main() !void {
//!     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//!     defer _ = gpa.deinit();
//!     const allocator = gpa.allocator();
//!
//!     // Compile a pattern
//!     var regex = try re.compile(allocator, "hello (\\w+)");
//!     defer regex.deinit();
//!
//!     // Match against text
//!     if (try regex.match("hello world")) |*m| {
//!         defer m.deinit();
//!         std.debug.print("Match: {s}\n", .{m.fullMatch()});
//!         if (m.get(1)) |group| {
//!             std.debug.print("Group 1: {s}\n", .{group});
//!         }
//!     }
//! }
//! ```
//!
//! ## Supported Features
//!
//! - Literals: plain text matching
//! - Character classes: `[abc]`, `[^abc]`, `[a-z]`, `\d`, `\w`, `\s`
//! - Quantifiers: `*`, `+`, `?`, `{n,m}`, greedy and non-greedy
//! - Anchors: `^` (start), `$` (end), `\b` (word boundary)
//! - Groups: capturing `()`, non-capturing `(?:...)`, named `(?P<name>...)`
//! - Alternation: `|` (OR operator)
//! - Backreferences: `\1`, `\2`, etc.

const regex = @import("src/regex.zig");

/// The main Regex type for compiling and matching patterns
pub const Regex = regex.Regex;

/// Match result containing captured groups and match information
pub const Match = @import("src/match.zig").Match;

/// Capture group information (start, end, optional name)
pub const Group = @import("src/match.zig").Group;

/// Result of a substitution operation containing the modified string and replacement count
pub const SubResult = @import("src/match.zig").SubResult;

/// Flags for controlling regex compilation and matching behavior
pub const RegexFlags = regex.RegexFlags;

/// Error types that can occur during regex operations
pub const RegexError = regex.RegexError;

/// Compile a regex pattern with default flags
///
/// The pattern string is duplicated and stored internally.
/// Returns a Regex that must be deinitialized with `regex.deinit()`.
///
/// ## Example
/// ```zig
/// var regex = try re.compile(allocator, "\\d+");
/// defer regex.deinit();
/// ```
pub const compile = Regex.compile;

/// Compile a regex pattern with specific flags
///
/// ## Example
/// ```zig
/// var flags = re.RegexFlags{ .ignore_case = true };
/// var regex = try re.compileWithFlags(allocator, "hello", flags);
/// defer regex.deinit();
/// ```
pub const compileWithFlags = Regex.compileWithFlags;

/// Match the pattern at the beginning of the text (with implicit ^ anchor)
///
/// Returns null if no match is found at the start.
/// The returned Match must be deinitialized with `match.deinit()`.
pub const match = Regex.match;

/// Search for a match anywhere in the text
///
/// Returns null if no match is found.
/// The returned Match must be deinitialized with `match.deinit()`.
pub const search = Regex.search;

/// Escape special regex characters in a string
///
/// Returns an allocated string where all special regex characters
/// (like `.`, `*`, `+`, etc.) are escaped with a backslash.
/// The caller owns the returned memory and must free it.
///
/// ## Example
/// ```zig
/// const escaped = try re.escape(allocator, "hello.world");
/// defer allocator.free(escaped);
/// // escaped == "hello\\.world"
/// ```
pub const escape = regex.escape;
