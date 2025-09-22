const std = @import("std");

const lib = @import("../mod.zig");

const Token = lib.token.Token;
const TokenKind = lib.token.TokenKind;
const Ast = lib.ast.Ast;
const AstState = lib.ast.AstState;

/// Recursive descent Pratt parser.
pub const Parser = struct {
    code: []const u8,
    tokens: []const Token,
    idx: usize,
    ast: Ast,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, code: []const u8, tokens: []const Token) Self {
        return .{
            .code = code,
            .tokens = tokens,
            .idx = 0,
            .ast = Ast.init(allocator, code),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.ast.deinit();
    }

    pub fn state(self: Self) ParserState {
        return ParserState{ .idx = self.idx, .astState = self.ast.state() };
    }

    pub fn reset(self: *Self, s: ParserState) void {
        self.idx = s.idx;
        self.ast.reset(s.astState);
    }

    pub fn text(self: Self, token: Token) []const u8 {
        const span = token.span;
        const string = self.code[span.start..span.end];
        return string;
    }

    pub fn currentText(self: Self) []const u8 {
        return self.text(self.tokens[self.idx]);
    }

    pub fn currentToken(self: Self) Token {
        return self.tokens[self.idx];
    }

    pub fn peekToken(self: Self) ?Token {
        if (self.idx + 1 < self.tokens.len) {
            return self.tokens[self.idx + 1];
        }
        return null;
    }

    pub fn advanceToken(self: *Self) void {
        self.idx += 1;
    }

    pub fn expectToken(self: Self, expected: TokenKind) !void {
        if (self.currentToken().kind != expected) {
            std.debug.print("Expected: {any}\n", .{expected});
            std.debug.print("Found: {any}\n", .{self.currentToken().kind});
        }
        try std.testing.expect(self.currentToken().kind == expected);
    }

    pub fn expectAdvanceToken(self: *Self, tokenKind: TokenKind) !void {
        try self.expectToken(tokenKind);
        self.advanceToken();
    }

    pub fn parseFile(self: *Self) !void {
        while (self.peekToken() != null) {
            const token = self.currentToken();
            switch (token.kind) {
                .FN => try self.parseFn(),
                else => {
                    std.debug.print("Found an unexpected token while parsing file node.\n", .{});
                    std.debug.print("Failed to parse token:\n  Kind: {any},\n  Span: {any} \n", .{
                        token.kind,
                        token.span,
                    });
                },
            }
        }
    }

    pub fn parseFn(self: *Self) !void {
        try self.expectAdvanceToken(.FN);
        const name = try self.parseName();
        try self.expectAdvanceToken(.LPAREN);
        try self.expectAdvanceToken(.RPAREN);
        const typex = try self.parseType();
        const block = try self.parseBlock();
        lib.ignore(.{ name, typex, block });
    }

    pub fn parseBlock(self: *Self) !void {
        try self.expectAdvanceToken(.LBRACE);
        while (self.currentToken().kind != .RBRACE) {
            try self.parseStmt();
        }
        try self.expectAdvanceToken(.RBRACE);
    }

    pub fn parseStmt(self: *Self) !void {
        const token = self.currentToken();
        switch (token.kind) {
            .LET => {
                try self.expectAdvanceToken(.LET);
                try self.parseVar();
                try self.expectAdvanceToken(.EQUAL);
                try self.parseExpr();
                try self.expectAdvanceToken(.SEMICOLON);
            },
            .RETURN => {
                try self.expectAdvanceToken(.RETURN);
                try self.parseExpr();
                try self.expectAdvanceToken(.SEMICOLON);
            },
            else => return,
        }
    }

    pub fn parseExpr(self: *Self) !void {
        const token = self.currentToken();
        switch (token.kind) {
            .IDVAL => {
                switch (self.peekToken().?.kind) {
                    .LPAREN => {
                        self.advanceToken();
                        try self.expectAdvanceToken(.LPAREN);
                        try self.expectAdvanceToken(.RPAREN);
                    },
                    else => self.advanceToken(),
                }
            },
            .INTVAL => {
                self.advanceToken();
            },
            else => unreachable,
        }
    }

    pub fn parseVar(self: *Self) !void {
        const name = try self.parseName();
        lib.ignore(name);
    }

    pub fn parseType(self: *Self) !void {
        const token = self.currentToken();
        switch (token.kind) {
            .INT => try self.expectAdvanceToken(.INT),
            else => unreachable,
        }
    }

    // FIXME: The index of the expression node is not correct. Fix this!
    pub fn parseName(self: *Self) !lib.ast.Expr {
        const name = self.currentText();
        try self.expectAdvanceToken(.IDVAL);
        return .{
            .idx = 0,
            .node = .{ .Name = .{ .idx = 0, .string = name } },
        };
    }
};

pub const ParserState = struct {
    idx: usize,
    astState: AstState,
};
