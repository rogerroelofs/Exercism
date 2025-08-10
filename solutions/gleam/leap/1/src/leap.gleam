import gleam/int

pub fn is_leap_year(year: Int) -> Bool {
  let assert Ok(is400) = int.remainder(year, 400)
  let assert Ok(is100) = int.remainder(year, 100)
  let assert Ok(is4) = int.remainder(year, 4)

  case year {
    _ if is400 == 0 -> True
    _ if is100 == 0 -> False
    _ if is4 == 0 -> True
    _ -> False
  }
}
