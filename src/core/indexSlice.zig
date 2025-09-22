const IndexSpan = @import("indexSpan.zig").IndexSpan;

/// Slice an array in range [start, end).
pub fn IndexSlice(comptime T: type) type {
    return struct {
        span: IndexSpan,
        slice: []const T,

        const Self = @This();

        pub fn init(data: []const T, start: usize, end: usize) Self {
            return .{ .span = IndexSpan.init(start, end), .slice = data[start..end] };
        }

        pub fn len(self: Self) usize {
            return self.span.len();
        }
    };
}
