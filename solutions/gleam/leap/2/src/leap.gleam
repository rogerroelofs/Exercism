pub fn is_leap_year(year: Int) -> Bool {
  let is400 = year % 400
  let is100 = year % 100
  let is4 = year % 4

  is4 == 0 && is100 != 0 || is400 == 0
}
