const std = @import("std");

const lib = @import("../mod.zig");
const str = lib.str;

const Token = @import("token.zig").Token;
const IndexSpan = @import("../core/indexSpan.zig").IndexSpan;

pub const Lexer = struct {
    code: []const u8,
    idx: u32,

    const Self = @This();

    pub fn init(code: []const u8) Self {
        return .{ .code = code, .idx = 0 };
    }

    pub fn reset(self: *Self) void {
        self.idx = 0;
    }

    pub fn next(self: Self) !struct { Token, Self } {
        const a = str.charAt(self.code, self.idx + 0) orelse ' ';
        const b = str.charAt(self.code, self.idx + 1) orelse ' ';
        const c = str.charAt(self.code, self.idx + 2) orelse ' ';
        const d = str.charAt(self.code, self.idx + 3) orelse ' ';
        const e = str.charAt(self.code, self.idx + 4) orelse ' ';
        const f = str.charAt(self.code, self.idx + 5) orelse ' ';
        const g = str.charAt(self.code, self.idx + 6) orelse ' ';
        const h = str.charAt(self.code, self.idx + 7) orelse ' ';
        const i = str.charAt(self.code, self.idx + 8) orelse ' ';

        const span1 = IndexSpan.init(self.idx, self.idx + 1);
        const span2 = IndexSpan.init(self.idx, self.idx + 2);
        const span3 = IndexSpan.init(self.idx, self.idx + 3);
        const span4 = IndexSpan.init(self.idx, self.idx + 4);
        const span5 = IndexSpan.init(self.idx, self.idx + 5);
        const span6 = IndexSpan.init(self.idx, self.idx + 6);
        const span8 = IndexSpan.init(self.idx, self.idx + 8);

        const none = Token{ .kind = .None, .span = span1 };

        const token = x: {
            var kind = none.kind;
            var span = none.span;

            const isdel = str.isDelimiter;

            switch (a) {
                ' ', '\n' => {
                    kind = .SPACE;
                    span = span1;
                },
                '\t' => {
                    // Tabs are not allowed for now (like Zig).
                    // Should they be allowed?
                },
                '\r' => {
                    // Carraige returns are not allowed for now.
                    // Should they be allowed?
                },
                'b' => {
                    if (b == 'r' and c == 'e' and d == 'a' and e == 'k' and isdel(f)) {
                        kind = .BREAK;
                        span = span5;
                    }
                },
                'c' => {
                    if (b == 'o' and c == 'n' and d == 't' and e == 'i' and f == 'n' and g == 'u' and h == 'e' and isdel(i)) {
                        kind = .CONTINUE;
                        span = span8;
                    } else if (b == 'o' and c == 'n' and isdel(d)) {
                        kind = .CON;
                        span = span3;
                    }
                },
                'e' => {
                    if (b == 'n' and c == 'u' and d == 'm' and isdel(e)) {
                        kind = .ENUM;
                        span = span4;
                    }
                },
                'f' => {
                    if (b == 'n' and isdel(c)) {
                        kind = .FN;
                        span = span2;
                    }
                    if (b == 'l' and c == 'o' and d == 'a' and e == 't' and isdel(f)) {
                        kind = .FLOAT;
                        span = span5;
                    }
                },
                'i' => {
                    if (b == 'n' and c == 't' and isdel(d)) {
                        kind = .INT;
                        span = span3;
                    }
                },
                'l' => {
                    if (b == 'e' and c == 't' and isdel(d)) {
                        kind = .LET;
                        span = span3;
                    }
                },
                'm' => {
                    if (b == 'u' and c == 't' and isdel(d)) {
                        kind = .MUT;
                        span = span3;
                    }
                },
                'r' => {
                    if (b == 'e' and c == 't' and d == 'u' and e == 'r' and f == 'n' and isdel(g)) {
                        kind = .RETURN;
                        span = span6;
                    }
                },
                's' => {
                    if (b == 't' and c == 'r' and d == 'u' and e == 'c' and f == 't' and isdel(g)) {
                        kind = .STRUCT;
                        span = span6;
                    } else if (b == 'e' and c == 't' and isdel(d)) {
                        kind = .SET;
                        span = span3;
                    }
                },
                'u' => {
                    if (b == 'n' and c == 'i' and d == 'o' and e == 'n' and isdel(f)) {
                        kind = .UNION;
                        span = span5;
                    }
                },
                'v' => {
                    if (b == 'o' and c == 'i' and d == 'd' and isdel(e)) {
                        kind = .VOID;
                        span = span4;
                    }
                },
                '{' => {
                    kind = .LBRACE;
                    span = span1;
                },
                '}' => {
                    kind = .RBRACE;
                    span = span1;
                },
                '[' => {
                    kind = .LBRACK;
                    span = span1;
                },
                ']' => {
                    kind = .RBRACK;
                    span = span1;
                },
                '(' => {
                    kind = .LPAREN;
                    span = span1;
                },
                ')' => {
                    kind = .RPAREN;
                    span = span1;
                },
                '=' => {
                    if (b == '=' and isdel(c)) {
                        kind = .EQUALEQUAL;
                        span = span2;
                    } else {
                        kind = .EQUAL;
                        span = span1;
                    }
                },
                ',' => {
                    kind = .COMMA;
                    span = span1;
                },
                ':' => {
                    kind = .COLON;
                    span = span1;
                },
                ';' => {
                    kind = .SEMICOLON;
                    span = span1;
                },

                else => {},
            }

            if (kind == .None) {
                if (str.isAlphabet(a) and a != ' ') {
                    const token = lexid(self.code[self.idx..], self.idx) orelse none;
                    kind = token.kind;
                    span = token.span;
                }
                if (str.isNumeric(a)) {
                    const token = lexnum(self.code[self.idx..], self.idx) orelse none;
                    kind = token.kind;
                    span = token.span;
                }
            }

            break :x Token{ .kind = kind, .span = span };
        };

        return .{
            token,
            Self{ .code = self.code, .idx = token.span.end },
        };
    }

    pub fn isEmpty(self: Self) bool {
        return self.idx >= self.code.len;
    }
};

fn lexid(code: []const u8, i: usize) ?Token {
    var iend = i;
    for (code) |a| {
        if (str.isDelimiter(a) and a != '_') {
            break;
        }
        iend += 1;
    }
    return .{ .kind = .IDVAL, .span = .init(i, iend) };
}

fn lexnum(code: []const u8, i: usize) ?Token {
    var iend = i;
    for (code) |a| {
        if (!str.isNumeric(a)) {
            break;
        }
        iend += 1;
    }
    return .{ .kind = .INTVAL, .span = .init(i, iend) };
}
