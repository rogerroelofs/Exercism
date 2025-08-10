const std = @import("std");

pub const Signal = enum {
    wink,
    double_blink,
    close_your_eyes,
    jump,
};

pub fn calculateHandshake(allocator: std.mem.Allocator, n: u5) ![]Signal {
    var res = std.ArrayList(Signal).init(allocator);
    inline for (std.enums.values(Signal), .{ 1, 2, 4, 8 }) |field, bit|
        if (n & bit > 0) try if (n & 16 > 0) res.insert(0, field) else res.append(field);
    return res.toOwnedSlice();
}