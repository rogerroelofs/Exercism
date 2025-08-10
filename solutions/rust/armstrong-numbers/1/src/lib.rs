pub fn is_armstrong_number(num: u32) -> bool {
    let digits = num.to_string().chars().map(|c| c.to_digit(10).unwrap()).collect::<Vec<u32>>();
    let power = digits.len() as u32;
    let sum = digits.iter().map(|&x| u64::from(x)).fold(0 as u64, |acc, x| acc + x.pow(power));
    sum == u64::from(num)
}
