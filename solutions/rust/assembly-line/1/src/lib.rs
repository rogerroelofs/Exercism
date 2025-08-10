pub fn production_rate_per_hour(speed: u8) -> f64 {
    let full = speed as f64 * 221.0;
    match speed {
        9 | 10 => full * 0.77,
        5..=8 => full * 0.9,
        _ => full,
    }
}

pub fn working_items_per_minute(speed: u8) -> u32 {
    (production_rate_per_hour(speed) / 60.0) as u32
}
