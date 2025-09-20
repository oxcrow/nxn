/// Integer index range of [start, end).
///
/// Note: `end` is not inclusive. Thus the real range is in [start, end-1].
/// However we store the span as [start, end), to allow simpler iterations.
///
/// That is because,
/// - It's easier to write `for (int i=0; i<end; i++)`,
/// - Instead of `for (int i=0; i<=end-1; i++)`
pub const IndexSpan = extern struct {
    start: u32,
    end: u32,

    const Self = @This();

    pub fn init(start: usize, end: usize) Self {
        return .{ .start = @truncate(start), .end = @truncate(end) };
    }

    pub fn len(self: Self) usize {
        return self.end - self.start;
    }
};
