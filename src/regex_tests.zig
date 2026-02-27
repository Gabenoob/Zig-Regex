//! Comprehensive test suite for Zig Regex library
//! Tests common use cases and patterns

const std = @import("std");
const re = @import("regex.zig");
const Regex = re.Regex;
const RegexError = re.RegexError;

const testing = std.testing;

// Helper function to check if a match exists and deinit it
fn matchExists(regex: *const Regex, text: []const u8) !bool {
    if (try regex.match(text)) |*m| {
        m.deinit();
        return true;
    }
    return false;
}

fn searchExists(regex: *const Regex, text: []const u8) !bool {
    if (try regex.search(text)) |*m| {
        m.deinit();
        return true;
    }
    return false;
}

// =============================================================================
// Basic Literal Matching Tests
// =============================================================================

test "literal match - simple text" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "hello");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "hello"));
    try testing.expect(try matchExists(&regex, "hello world"));
    try testing.expect(!try matchExists(&regex, "say hello"));
}

test "literal match - case sensitivity" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "Hello");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "Hello"));
    try testing.expect(!try matchExists(&regex, "hello"));
}

test "literal match - special characters as literals" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "hello\\.world");
    defer regex.deinit();

    if (try regex.match("hello.world")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("hello.world", m.fullMatch());
    } else {
        try testing.expect(false); // Should match
    }
}

// =============================================================================
// Character Classes Tests
// =============================================================================

test "character class - simple [abc]" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "[aeiou]");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "a"));
    try testing.expect(try matchExists(&regex, "e"));
    try testing.expect(!try matchExists(&regex, "x"));
}

test "character class - negated [^abc]" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "[^aeiou]");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "b"));
    try testing.expect(try matchExists(&regex, "x"));
    try testing.expect(!try matchExists(&regex, "a"));
}

test "character class - ranges [a-z]" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "[a-z]+");
    defer regex.deinit();

    if (try regex.match("abc")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("abc", m.fullMatch());
    } else try testing.expect(false);

    try testing.expect(!try matchExists(&regex, "ABC"));
}

test "character class - multiple ranges [a-zA-Z]" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "[a-zA-Z]+");
    defer regex.deinit();

    if (try regex.match("HelloWorld")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("HelloWorld", m.fullMatch());
    } else try testing.expect(false);
}

test "character class - digits \\d" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\d+");
    defer regex.deinit();

    if (try regex.match("12345")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("12345", m.fullMatch());
    } else try testing.expect(false);

    try testing.expect(!try matchExists(&regex, "abc"));
}

test "character class - word characters \\w" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\w+");
    defer regex.deinit();

    if (try regex.match("hello_123")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("hello_123", m.fullMatch());
    } else try testing.expect(false);

    try testing.expect(!try matchExists(&regex, "   "));
}

test "character class - whitespace \\s" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\s+");
    defer regex.deinit();

    if (try regex.match("   ")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("   ", m.fullMatch());
    } else try testing.expect(false);

    try testing.expect(!try matchExists(&regex, "abc"));
}

test "character class - negated digits \\D" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\D+");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "abc"));
    try testing.expect(!try matchExists(&regex, "123"));
}

test "character class - negated word \\W" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\W+");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "@#$%"));
    try testing.expect(!try matchExists(&regex, "abc"));
}

test "character class - negated space \\S" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\S+");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "abc"));
    try testing.expect(!try matchExists(&regex, "   "));
}

// =============================================================================
// Quantifier Tests
// =============================================================================

test "quantifier - zero or more *" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a*");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, ""));
    try testing.expect(try matchExists(&regex, "a"));
    try testing.expect(try matchExists(&regex, "aaa"));
}

test "quantifier - one or more +" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a+");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "a"));
    try testing.expect(try matchExists(&regex, "aaa"));
    try testing.expect(!try matchExists(&regex, ""));
}

test "quantifier - zero or one ?" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a?");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, ""));
    try testing.expect(try matchExists(&regex, "a"));
    // Should match only first 'a', rest stays unmatched
    if (try regex.match("aa")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("a", m.fullMatch());
    } else try testing.expect(false);
}

test "quantifier - exact count {n}" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a{3}");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "aaa"));
    try testing.expect(!try matchExists(&regex, "aa"));
    try testing.expect(try matchExists(&regex, "aaaa")); // Matches first 3
}

test "quantifier - minimum {n,}" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a{2,}");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "aa"));
    try testing.expect(try matchExists(&regex, "aaaa"));
    try testing.expect(!try matchExists(&regex, "a"));
}

test "quantifier - range {n,m}" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a{2,4}");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "aa"));
    try testing.expect(try matchExists(&regex, "aaa"));
    try testing.expect(try matchExists(&regex, "aaaa"));
    try testing.expect(!try matchExists(&regex, "a"));
}

test "quantifier - combined with character class" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "[0-9]+");
    defer regex.deinit();

    if (try regex.match("12345")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("12345", m.fullMatch());
    } else try testing.expect(false);
}

// =============================================================================
// Anchor Tests
// =============================================================================

test "anchor - start ^" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "^hello");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "hello world"));
    try testing.expect(try searchExists(&regex, "say hello"));
}

test "anchor - both ^ and $" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "^hello$");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "hello"));
}

test "anchor - word boundary \\b" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\bcat\\b");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "cat"));
    try testing.expect(try searchExists(&regex, "a cat here"));
}

// =============================================================================
// Group Tests
// =============================================================================

test "group - simple capturing" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(\\w+)");
    defer regex.deinit();

    if (try regex.match("hello")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("hello", m.fullMatch());
        try testing.expectEqualStrings("hello", m.get(1).?);
    } else try testing.expect(false);
}

test "group - multiple capturing" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(\\w+)=([0-9]+)");
    defer regex.deinit();

    if (try regex.match("age=25")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("age=25", m.fullMatch());
        try testing.expectEqualStrings("age", m.get(1).?);
        try testing.expectEqualStrings("25", m.get(2).?);
    } else try testing.expect(false);
}

test "group - nested groups" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "((\\w+)-([0-9]+))");
    defer regex.deinit();

    if (try regex.match("item-42")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("item-42", m.fullMatch());
        try testing.expectEqualStrings("item-42", m.get(1).?);
        try testing.expectEqualStrings("item", m.get(2).?);
        try testing.expectEqualStrings("42", m.get(3).?);
    } else try testing.expect(false);
}

test "group - non-capturing (?:...)" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(?:ab)+(c)");
    defer regex.deinit();

    if (try regex.match("abc")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("abc", m.fullMatch());
        // Non-capturing group doesn't count
        try testing.expectEqualStrings("c", m.get(1).?);
    } else try testing.expect(false);
}

test "group - with quantifiers" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(ab)+");
    defer regex.deinit();

    if (try regex.match("abab")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("abab", m.fullMatch());
        try testing.expectEqualStrings("ab", m.get(1).?); // Last capture
    } else try testing.expect(false);
}

test "group - optional" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(www\\.)?example\\.com");
    defer regex.deinit();

    if (try regex.match("www.example.com")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("www.example.com", m.fullMatch());
        try testing.expectEqualStrings("www.", m.get(1).?);
    } else try testing.expect(false);
}

// =============================================================================
// Alternation Tests
// =============================================================================

test "alternation - simple a|b" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "cat|dog");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "cat"));
    try testing.expect(try matchExists(&regex, "dog"));
    try testing.expect(!try matchExists(&regex, "bird"));
}

test "alternation - with groups" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(cat|dog)s?");
    defer regex.deinit();

    if (try regex.match("cat")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("cat", m.fullMatch());
        try testing.expectEqualStrings("cat", m.get(1).?);
    } else try testing.expect(false);

    if (try regex.match("cats")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("cats", m.fullMatch());
        try testing.expectEqualStrings("cat", m.get(1).?);
    } else try testing.expect(false);
}

test "alternation - complex" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\b(hello|hi|hey)\\b");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "say hello"));
    try testing.expect(try searchExists(&regex, "say hi"));
    try testing.expect(try searchExists(&regex, "say hey"));
}

// =============================================================================
// Dot (Any Character) Tests
// =============================================================================

test "dot - matches any char" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a.c");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "abc"));
    try testing.expect(try matchExists(&regex, "a.c"));
    try testing.expect(try matchExists(&regex, "axc"));
    try testing.expect(!try matchExists(&regex, "ac"));
}

test "dot - with quantifier" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, ".*");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, ""));
    try testing.expect(try matchExists(&regex, "anything"));
    try testing.expect(try matchExists(&regex, "123 abc XYZ"));
}

// =============================================================================
// Backreference Tests
// =============================================================================

test "backreference - simple \\1" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(.)\\1");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "aa"));
    try testing.expect(try matchExists(&regex, "bb"));
    try testing.expect(!try matchExists(&regex, "ab"));
}

test "backreference - word repetition" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\b(\\w+)\\s+\\1\\b");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "the the"));
    try testing.expect(try searchExists(&regex, "hello hello"));
}

// =============================================================================
// Search Tests
// =============================================================================

test "search - finds anywhere in text" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "world");
    defer regex.deinit();

    if (try regex.search("hello world here")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("world", m.fullMatch());
        try testing.expectEqual(@as(usize, 6), m.start);
        try testing.expectEqual(@as(usize, 11), m.end);
    } else try testing.expect(false);
}

test "search - multiple occurrences finds first" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "o");
    defer regex.deinit();

    if (try regex.search("hello world")) |*m| {
        defer m.deinit();
        try testing.expectEqual(@as(usize, 4), m.start); // First 'o' in hello
    } else try testing.expect(false);
}

test "search - no match" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "xyz");
    defer regex.deinit();

    try testing.expect(try regex.search("hello world") == null);
}

// =============================================================================
// findAll Tests
// =============================================================================

test "findAll - all words" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\w+");
    defer regex.deinit();

    const matches = try regex.findAll("hello world test");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    try testing.expectEqual(@as(usize, 3), matches.len);
    try testing.expectEqualStrings("hello", matches[0].fullMatch());
    try testing.expectEqualStrings("world", matches[1].fullMatch());
    try testing.expectEqualStrings("test", matches[2].fullMatch());
}

test "findAll - all numbers" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\d+");
    defer regex.deinit();

    const matches = try regex.findAll("abc 123 def 456 ghi 789");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    try testing.expectEqual(@as(usize, 3), matches.len);
    try testing.expectEqualStrings("123", matches[0].fullMatch());
    try testing.expectEqualStrings("456", matches[1].fullMatch());
    try testing.expectEqualStrings("789", matches[2].fullMatch());
}

test "findAll - overlapping not allowed" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "aba");
    defer regex.deinit();

    const matches = try regex.findAll("ababa");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    // Should find only 1 match, not 2 overlapping
    try testing.expectEqual(@as(usize, 1), matches.len);
}

test "findAll - empty result" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "xyz");
    defer regex.deinit();

    const matches = try regex.findAll("hello world");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    try testing.expectEqual(@as(usize, 0), matches.len);
}

test "findAll - with groups" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(\\w)=(\\d)");
    defer regex.deinit();

    const matches = try regex.findAll("a=1 b=2 c=3");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    try testing.expectEqual(@as(usize, 3), matches.len);
    try testing.expectEqualStrings("a", matches[0].get(1).?);
    try testing.expectEqualStrings("1", matches[0].get(2).?);
}

// =============================================================================
// Substitution Tests (sub and subAll)
// =============================================================================

test "sub - replace first occurrence" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "cat");
    defer regex.deinit();

    const result = try regex.sub("dog", "the cat sat on the cat");
    defer allocator.free(result);

    try testing.expectEqualStrings("the dog sat on the cat", result);
}

test "sub - no match returns copy" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "xyz");
    defer regex.deinit();

    const result = try regex.sub("abc", "hello world");
    defer allocator.free(result);

    try testing.expectEqualStrings("hello world", result);
}

test "subAll - replace all occurrences" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a");
    defer regex.deinit();

    const result = try regex.subAll("o", "banana");
    defer allocator.free(result);

    try testing.expectEqualStrings("bonono", result);
}

test "subAll - complex replacement" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\s+");
    defer regex.deinit();

    const result = try regex.subAll(" ", "hello    world     test");
    defer allocator.free(result);

    try testing.expectEqualStrings("hello world test", result);
}

// =============================================================================
// Split Tests
// =============================================================================

test "split - by whitespace" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\s+");
    defer regex.deinit();

    const parts = try regex.split("hello   world  test");
    defer {
        for (parts) |p| allocator.free(p);
        allocator.free(parts);
    }

    try testing.expectEqual(@as(usize, 3), parts.len);
    try testing.expectEqualStrings("hello", parts[0]);
    try testing.expectEqualStrings("world", parts[1]);
    try testing.expectEqualStrings("test", parts[2]);
}

test "split - by comma" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, ",");
    defer regex.deinit();

    const parts = try regex.split("a,b,c,d");
    defer {
        for (parts) |p| allocator.free(p);
        allocator.free(parts);
    }

    // Split by comma should produce ["a", "b", "c", "d"]
    try testing.expectEqual(@as(usize, 4), parts.len);
    try testing.expectEqualStrings("a", parts[0]);
    try testing.expectEqualStrings("b", parts[1]);
    try testing.expectEqualStrings("c", parts[2]);
    try testing.expectEqualStrings("d", parts[3]);
}

test "split - empty parts preserved" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, ",");
    defer regex.deinit();

    const parts = try regex.split("a,,c");
    defer {
        for (parts) |p| allocator.free(p);
        allocator.free(parts);
    }

    // Split "a,,c" by comma should produce ["a", "", "c"] with empty middle part
    try testing.expectEqual(@as(usize, 3), parts.len);
    try testing.expectEqualStrings("a", parts[0]);
    try testing.expectEqualStrings("", parts[1]); // Empty part
    try testing.expectEqualStrings("c", parts[2]);
}

test "split - no match returns single element" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "xyz");
    defer regex.deinit();

    const parts = try regex.split("hello world");
    defer {
        for (parts) |p| allocator.free(p);
        allocator.free(parts);
    }

    try testing.expectEqual(@as(usize, 1), parts.len);
    try testing.expectEqualStrings("hello world", parts[0]);
}

// =============================================================================
// Match Span and Position Tests
// =============================================================================

test "match span" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "world");
    defer regex.deinit();

    if (try regex.search("hello world here")) |*m| {
        defer m.deinit();
        const span = m.span();
        try testing.expectEqual(@as(usize, 6), span[0]);
        try testing.expectEqual(@as(usize, 11), span[1]);
    } else try testing.expect(false);
}

test "match groupSpan" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "hello (\\w+)");
    defer regex.deinit();

    if (try regex.match("hello world")) |*m| {
        defer m.deinit();

        const full_span = m.groupSpan(0).?;
        try testing.expectEqual(@as(usize, 0), full_span[0]);
        try testing.expectEqual(@as(usize, 11), full_span[1]);

        const group_span = m.groupSpan(1).?;
        try testing.expectEqual(@as(usize, 6), group_span[0]);
        try testing.expectEqual(@as(usize, 11), group_span[1]);
    } else try testing.expect(false);
}

test "match len" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "hello");
    defer regex.deinit();

    if (try regex.match("hello")) |*m| {
        defer m.deinit();
        try testing.expectEqual(@as(usize, 5), m.len());
    } else try testing.expect(false);
}

// =============================================================================
// Escape Function Tests
// =============================================================================

test "escape - special chars" {
    const allocator = testing.allocator;

    const escaped = try re.escape(allocator, "hello.world");
    defer allocator.free(escaped);

    try testing.expectEqualStrings("hello\\.world", escaped);
}

test "escape - multiple specials" {
    const allocator = testing.allocator;

    const escaped = try re.escape(allocator, "a+b*c?");
    defer allocator.free(escaped);

    try testing.expectEqualStrings("a\\+b\\*c\\?", escaped);
}

test "escape - brackets" {
    const allocator = testing.allocator;

    const escaped = try re.escape(allocator, "[test]");
    defer allocator.free(escaped);

    try testing.expectEqualStrings("\\[test\\]", escaped);
}

// =============================================================================
// Error Handling Tests
// =============================================================================

test "error - unmatched parenthesis" {
    const allocator = testing.allocator;
    const result = Regex.compile(allocator, "(abc");
    // Parser now properly returns UnmatchedParenthesis and cleans up memory on error
    try testing.expectError(RegexError.UnmatchedParenthesis, result);
}

test "error - unmatched bracket" {
    const allocator = testing.allocator;
    const result = Regex.compile(allocator, "[abc");
    // Parser now properly returns UnmatchedBracket and cleans up memory on error
    try testing.expectError(RegexError.UnmatchedBracket, result);
}

// =============================================================================
// Real-World Pattern Tests
// =============================================================================

test "real world - email pattern" {
    const allocator = testing.allocator;
    // Simplified email pattern (may need adjustment based on implementation)
    var regex = try Regex.compile(allocator, "\\w+@\\w+\\.\\w+");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "user@example.com"));
    // Note: Complex email patterns may vary in support
}

test "real world - phone number pattern" {
    const allocator = testing.allocator;
    // Pattern for ###-####
    var regex = try Regex.compile(allocator, "\\d{3}-\\d{4}");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "555-1234"));
    try testing.expect(try searchExists(&regex, "Call 555-1234 now"));
}

test "real world - date pattern (YYYY-MM-DD)" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\d{4}-\\d{2}-\\d{2}");
    defer regex.deinit();

    if (try regex.search("Date: 2024-03-15")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("2024-03-15", m.fullMatch());
    } else try testing.expect(false);
}

test "real world - URL pattern" {
    const allocator = testing.allocator;
    // Simple URL pattern
    var regex = try Regex.compile(allocator, "https?://[^\\s]+");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "Visit https://example.com for more"));
    try testing.expect(try searchExists(&regex, "Visit http://test.org/page"));
}

test "real world - IPv4 address" {
    const allocator = testing.allocator;
    // Pattern for IPv4 (simplified, doesn't validate range)
    var regex = try Regex.compile(allocator, "(?:\\d{1,3}\\.){3}\\d{1,3}");
    defer regex.deinit();

    if (try regex.search("Server at 192.168.1.1")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("192.168.1.1", m.fullMatch());
    } else try testing.expect(false);
}

test "real world - quoted string" {
    const allocator = testing.allocator;
    // Pattern for double-quoted string (simplified)
    var regex = try Regex.compile(allocator, "\"[^\"]*\"");
    defer regex.deinit();

    if (try regex.search("say \"hello world\" here")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("\"hello world\"", m.fullMatch());
    } else try testing.expect(false);
}

test "real world - hex color code" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "#[0-9a-fA-F]{6}");
    defer regex.deinit();

    if (try regex.search("Color: #FF5732")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("#FF5732", m.fullMatch());
    } else try testing.expect(false);

    try testing.expect(!try searchExists(&regex, "Color: #GGGGGG"));
}

test "real world - variable name (identifier)" {
    const allocator = testing.allocator;
    // Pattern for valid C-style identifiers
    var regex = try Regex.compile(allocator, "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "int _myVar = 5"));
    try testing.expect(try searchExists(&regex, "var123"));
}

test "real world - credit card (simplified)" {
    const allocator = testing.allocator;
    // Pattern for 4 groups of 4 digits (simplified)
    var regex = try Regex.compile(allocator, "\\d{4}-\\d{4}-\\d{4}-\\d{4}");
    defer regex.deinit();

    try testing.expect(try searchExists(&regex, "Card: 1234-5678-9012-3456"));
    // Note: Optional patterns in groups may have implementation-specific behavior
}

// =============================================================================
// Edge Cases and Stress Tests
// =============================================================================

test "edge case - very long pattern" {
    const allocator = testing.allocator;
    var pattern: std.ArrayList(u8) = .empty;
    defer pattern.deinit(allocator);

    // Create pattern "aaaaaaaa...a" (100 a's)
    for (0..100) |_| {
        try pattern.append(allocator, 'a');
    }

    var regex = try Regex.compile(allocator, pattern.items);
    defer regex.deinit();

    // Create matching text
    var text: std.ArrayList(u8) = .empty;
    defer text.deinit(allocator);
    for (0..100) |_| {
        try text.append(allocator, 'a');
    }

    try testing.expect(try matchExists(&regex, text.items));
}

test "edge case - unicode chars" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "hello");
    defer regex.deinit();

    // Should still work with ASCII patterns
    if (try regex.search("hello 世界")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("hello", m.fullMatch());
    } else try testing.expect(false);
}

test "edge case - empty text" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a*");
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, ""));
}

test "edge case - empty match" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "a*");
    defer regex.deinit();

    const matches = try regex.findAll("bbb");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    // Should find empty matches between chars
    try testing.expect(matches.len > 0);
}

test "edge case - many groups" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(a)(b)(c)(d)(e)");
    defer regex.deinit();

    if (try regex.match("abcde")) |*m| {
        defer m.deinit();
        try testing.expectEqual(@as(usize, 6), m.groupCount()); // 0-5
        try testing.expectEqualStrings("a", m.get(1).?);
        try testing.expectEqualStrings("b", m.get(2).?);
        try testing.expectEqualStrings("c", m.get(3).?);
        try testing.expectEqualStrings("d", m.get(4).?);
        try testing.expectEqualStrings("e", m.get(5).?);
    } else try testing.expect(false);
}

test "edge case - deeply nested groups" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(((a)))");
    defer regex.deinit();

    if (try regex.match("a")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("a", m.fullMatch());
        try testing.expectEqualStrings("a", m.get(1).?);
        try testing.expectEqualStrings("a", m.get(2).?);
        try testing.expectEqualStrings("a", m.get(3).?);
    } else try testing.expect(false);
}

// =============================================================================
// RegexFlags Tests
// =============================================================================

test "flags - case insensitive" {
    const allocator = testing.allocator;
    const flags = re.RegexFlags{ .ignore_case = true };
    var regex = try Regex.compileWithFlags(allocator, "hello", flags);
    defer regex.deinit();

    try testing.expect(try matchExists(&regex, "HELLO"));
    try testing.expect(try matchExists(&regex, "Hello"));
    try testing.expect(try matchExists(&regex, "hElLo"));
}

test "flags - fromString" {
    const flags = re.RegexFlags.fromString("ims");
    try testing.expect(flags.ignore_case);
    try testing.expect(flags.multiline);
    try testing.expect(flags.dotall);
    try testing.expect(!flags.verbose);
}

// =============================================================================
// Group Count Tests
// =============================================================================

test "groupCount - no groups" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "hello");
    defer regex.deinit();

    // Should still have group 0 (full match)
    try testing.expectEqual(@as(usize, 1), regex.groupCount());
}

test "groupCount - with groups" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(a)(b)(?:c)");
    defer regex.deinit();

    // Group 0 + 2 capturing groups (non-capturing doesn't count)
    try testing.expectEqual(@as(usize, 3), regex.groupCount());
}

// =============================================================================
// Invalid Access Tests
// =============================================================================

test "invalid - get non-existent group" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(a)");
    defer regex.deinit();

    if (try regex.match("a")) |*m| {
        defer m.deinit();
        try testing.expect(m.get(99) == null);
        try testing.expect(m.get(0) != null); // Full match always exists
    } else try testing.expect(false);
}

test "invalid - groupSpan non-existent" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(a)");
    defer regex.deinit();

    if (try regex.match("a")) |*m| {
        defer m.deinit();
        try testing.expect(m.groupSpan(99) == null);
    } else try testing.expect(false);
}

// =============================================================================
// Complex Integration Tests
// =============================================================================

test "integration - parse key=value pairs" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "(\\w+)=(\\w+)");
    defer regex.deinit();

    const matches = try regex.findAll("name=John age=30 city=NYC");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    try testing.expectEqual(@as(usize, 3), matches.len);
    try testing.expectEqualStrings("name", matches[0].get(1).?);
    try testing.expectEqualStrings("John", matches[0].get(2).?);
    try testing.expectEqualStrings("age", matches[1].get(1).?);
    try testing.expectEqualStrings("30", matches[1].get(2).?);
}

test "integration - parse CSV-like fields" {
    const allocator = testing.allocator;
    // Use a simple pattern that matches words
    var regex = try Regex.compile(allocator, "\\w+");
    defer regex.deinit();

    const matches = try regex.findAll("apple,banana,cherry");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    // Should find "apple", "banana", "cherry" as separate word matches
    try testing.expectEqual(@as(usize, 3), matches.len);
    try testing.expectEqualStrings("apple", matches[0].fullMatch());
    try testing.expectEqualStrings("banana", matches[1].fullMatch());
    try testing.expectEqualStrings("cherry", matches[2].fullMatch());
}

test "integration - extract hashtags" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "#[a-zA-Z0-9_]+");
    defer regex.deinit();

    const matches = try regex.findAll("Hello #world and #ZigLang!");
    defer {
        for (matches) |*m| m.deinit();
        allocator.free(matches);
    }

    try testing.expectEqual(@as(usize, 2), matches.len);
    try testing.expectEqualStrings("#world", matches[0].fullMatch());
    try testing.expectEqualStrings("#ZigLang", matches[1].fullMatch());
}

test "integration - normalize whitespace" {
    const allocator = testing.allocator;
    var regex = try Regex.compile(allocator, "\\s+");
    defer regex.deinit();

    const result = try regex.subAll(" ", "hello   \t\n  world  ");
    defer allocator.free(result);

    try testing.expectEqualStrings("hello world ", result);
}

test "integration - extract domain from email" {
    const allocator = testing.allocator;
    // Pattern to capture user and domain
    var regex = try Regex.compile(allocator, "([^@]+)@([^@]+)");
    defer regex.deinit();

    if (try regex.match("user@example.com")) |*m| {
        defer m.deinit();
        try testing.expectEqualStrings("user", m.get(1).?);
        try testing.expectEqualStrings("example.com", m.get(2).?);
    } else try testing.expect(false);
}
