const std = @import("std");

pub fn charAt(string: []const u8, i: usize) ?u8 {
    if (i < string.len) {
        return string[i];
    }
    return null;
}

pub fn isDelimiter(c: u8) bool {
    switch (c) {
        'a'...'z' => return false,
        'A'...'Z' => return false,
        '0'...'9' => return false,
        else => return true,
    }
    unreachable;
}

pub fn isAlphabet(c: u8) bool {
    switch (c) {
        'a'...'z' => return true,
        'A'...'Z' => return true,
        else => return false,
    }
    unreachable;
}

pub fn isNumeric(c: u8) bool {
    switch (c) {
        '0'...'9' => return true,
        else => return false,
    }
    unreachable;
}

pub fn isAlphaNumeric(c: u8) bool {
    if (isAlphabet(c) or isNumeric(c)) {
        return true;
    }
    return false;
}
