/// Ignore anything that is passed to this function.
///
/// Used to silence the annoying "unused variable", "unused argument" errors.
///
/// ```zig
/// var x: [32]u8 = undefined;
/// const y: usize = 0;
/// ignore(.{&x, y});     // Ignore multiple values with .{ } tuple syntax.
/// ignore(&x);           // Ignore mutable variables only by & reference.
/// ignore(&y);           // Ignore constants by value or by & reference.
/// ignore(y);            // Ignore constants by value or by & reference.
/// ```
pub fn ignore(x: anytype) void {
    _ = x;
}
