/// Writes a reversed copy of `s` to `buffer`.
pub fn reverse(buffer: []u8, s: []const u8) []u8 {
    var i: usize = s.len;
    var b: usize = 0;

    while (i > 0) : (i -= 1) {
        buffer[b] = s[i - 1];
        b = b + 1;
    }
    return buffer;
}
