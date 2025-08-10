import gleam/regex
import gleam/string

pub fn hey(remark: String) -> String {
  let trimmed = string.trim(remark)
  let assert Ok(yell) = regex.from_string("^[^A-Za-z]*[A-Z][^a-z]+$")
  let assert Ok(q) = regex.from_string("^.+\\?$")
  let is_empty = string.is_empty(trimmed)
  let is_yell = regex.check(with: yell, content: trimmed)
  let is_q = regex.check(with: q, content: trimmed)
  case trimmed {
    _ if is_empty -> "Fine. Be that way!"
    _ if is_yell && is_q -> "Calm down, I know what I'm doing!"
    _ if is_q -> "Sure."
    _ if is_yell -> "Whoa, chill out!"
    _ -> "Whatever."
  }
}
