const std = @import("std");

pub fn ArrayList(comptime T: type) type {
    return struct {
        array: std.ArrayList(T),

        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{ .array = std.ArrayList(T).init(allocator) };
        }

        pub fn initCapacity(allocator: std.mem.Allocator, n: usize) !Self {
            return Self{ .array = try std.ArrayList(T).initCapacity(allocator, n) };
        }

        pub fn deinit(self: Self) void {
            self.array.deinit();
        }

        pub fn len(self: Self) usize {
            return self.array.items.len;
        }

        pub fn capacity(self: Self) usize {
            return self.array.capacity;
        }

        pub fn append(self: *Self, x: T) !void {
            try self.array.append(x);
        }

        pub fn getLast(self: Self) T {
            return self.array.getLast();
        }

        pub fn at(self: Self, i: usize) T {
            return self.array.items[i];
        }

        pub fn in(self: Self, i: usize) *T {
            return &self.array.items[i];
        }
    };
}
