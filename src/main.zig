const std = @import("std");

const lib = @import("nxn_lib");
const ignore = lib.ignore;

fn dev(allocator: std.mem.Allocator) !void {
    ignore(.{allocator});
    lib.shutdown();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    const allocator = gpa.allocator();

    try dev(allocator);

    const leaked = gpa.detectLeaks();
    if (leaked) {
        std.debug.print("Has memory leaks: {any}\n", .{leaked});
    }
}
