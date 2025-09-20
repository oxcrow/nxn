const std = @import("std");

/// Allocator that can be used in C.
///
/// To use this allocator,
/// - Create the allocator in C.
/// - Pass the allocator to the required functions.
///
/// Note: For memory safety, each individual instance of the runtime,
/// and each thread invoked by each instance of the runtime,
/// will require a separate allocator.
const ExternAllocator = extern struct {
    context: [@sizeOf(std.mem.Allocator)]u8 align(@alignOf(std.mem.Allocator)),

    const Self = @This();

    fn init() Self {
        var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
        const allocator = gpa.allocator();
        return initWith(allocator);
    }

    fn initWith(allocator: std.mem.Allocator) Self {
        var self = ExternAllocator{ .context = undefined };
        const bytes = std.mem.asBytes(&allocator);
        for (bytes, 0..) |value, i| {
            self.context[i] = value;
        }
        return self;
    }

    fn alloc(self: Self, comptime T: type, n: usize) ![]T {
        const allocator = std.mem.bytesAsValue(std.mem.Allocator, &self.context);
        return try allocator.alloc(T, n);
    }

    fn free(self: Self, memory: anytype) void {
        const allocator = std.mem.bytesAsValue(std.mem.Allocator, &self.context);
        allocator.free(memory);
    }
};
