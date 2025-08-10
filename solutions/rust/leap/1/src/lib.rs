pub fn is_leap_year(year: u64) -> bool {
    if year.rem_euclid(400) == 0 { return true; }
    if year.rem_euclid(100) == 0 { return false; }
    year.rem_euclid(4) == 0
}
