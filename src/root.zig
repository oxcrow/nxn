const std = @import("std");

pub const lib = @import("mod.zig");
pub const ignore = lib.ignore;

pub fn shutdown() void {
    std.debug.print("+\n", .{});
}
