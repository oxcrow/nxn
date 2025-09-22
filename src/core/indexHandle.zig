const IndexSpan = @import("indexSpan.zig").IndexSpan;

/// A pointer handle to an indexed location of an array.
pub fn IndexHandle(comptime T: type) type {
    return struct {
        idx: u32,
        ptr: *const T,

        const Self = @This();

        pub fn init(data: *const T, i: usize) Self {
            return .{ .idx = i, .ptr = data };
        }
    };
}
