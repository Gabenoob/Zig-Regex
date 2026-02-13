const regex = @import("src/regex.zig");

pub const Regex = regex.Regex;
pub const Match = match.Match;
pub const Group = match.Group;
pub const SubResult = match.SubResult;
pub const RegexFlags = regex.RegexFlags;

pub const RegexError = regex.RegexError;

pub const compile = Regex.compile;
pub const compileWithFlags = Regex.compileWithFlags;
pub const match = Regex.match;
pub const search = Regex.search;

pub const escape = regex.escape;
