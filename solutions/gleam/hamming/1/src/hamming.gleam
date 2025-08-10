import gleam/list
import gleam/string

pub fn distance(strand1: String, strand2: String) -> Result(Int, Nil) {
  case string.length(strand1) == string.length(strand2) {
    True ->
      list.zip(string.to_graphemes(strand1), string.to_graphemes(strand2))
      |> list.fold(0, count_differences)
      |> Ok()
    False -> Error(Nil)
  }
}

fn count_differences(acc: Int, pair: #(String, String)) -> Int {
  let #(left, right) = pair
  case left == right {
    True -> acc
    False -> acc + 1
  }
}
