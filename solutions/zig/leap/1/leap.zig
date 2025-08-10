pub fn isLeapYear(year: u32) bool {
    const one_hundred: u32 = 100;
    const four_hundred: u32 = 400;
    const four: u32 = 4;
    return (if (@mod(year, one_hundred) == 0)
        @mod(year, four_hundred) == 0
    else
        @mod(year, four) == 0);
}
