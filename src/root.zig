const std = @import("std");

pub const lib = @import("mod.zig");

pub fn compile(allocator: std.mem.Allocator, file: []const u8) !void {
    const code = try std.fs.cwd().readFileAlloc(allocator, file, 1e7);
    defer allocator.free(code);

    const tokens = x: {
        var lexer = lib.lexer.Lexer.init(code);
        var tokens = try lib.ArrayList(lib.token.Token).initCapacity(allocator, code.len / 2);
        errdefer tokens.deinit();

        while (!lexer.isEmpty()) {
            const token, lexer = try lexer.next();
            if (token.kind != .SPACE and token.kind != .None) {
                try tokens.append(token);
            }
        }
        lexer.reset();

        break :x tokens;
    };
    defer tokens.deinit();

    const ast = x: {
        var parser = lib.parser.Parser.init(allocator, code, tokens.array.items);
        errdefer parser.deinit();

        try parser.parseFile();

        break :x 0;
    };
    lib.ignore(.{ast});
}
