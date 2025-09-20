const std = @import("std");

const lib = @import("nxn_lib");
const ignore = lib.lib.ignore;

fn dev(allocator: std.mem.Allocator) !void {
    try lib.compile(allocator, "src/x.nxn");
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    const allocator = gpa.allocator();

    try dev(allocator);

    const leaked = gpa.detectLeaks();
    if (leaked) {
        std.debug.print("Has memory leaks!\n", .{});
    }
}
