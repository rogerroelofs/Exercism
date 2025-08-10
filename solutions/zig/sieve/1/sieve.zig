const std = @import("std");

pub fn primes(buffer: []u32, comptime limit: u32) []u32 {
    var bits = std.StaticBitSet(limit + 1).initEmpty();
    bits.set(0);
    bits.set(1);

    var idx: u32 = 0;
    var i: u32 = 2;

    while (i <= limit) : (i += 1) {
        if (bits.isSet(i)) continue;

        buffer[idx] = i;
        idx += 1;

        var j = i * i;
        while (j <= limit) : (j += i) {
            bits.set(j);
        }
    }

    return buffer[0..idx];
}