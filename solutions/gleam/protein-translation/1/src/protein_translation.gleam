import gleam/string
import gleam/list

pub fn proteins(rna: String) -> Result(List(String), Nil) {
  let proteins =
    rna
    |> string.to_graphemes
    |> list.sized_chunk(3)
    |> list.map(string.concat)
    |> list.take_while(not_stop_condon)
    |> list.map(get_protein)

  case list.contains(proteins, "") {
    True -> Error(Nil)
    _ -> Ok(proteins)
  }
}

fn get_protein(condon: String) {
  case condon {
    "AUG" -> "Methionine"
    "UUU" | "UUC" -> "Phenylalanine"
    "UUA" | "UUG" -> "Leucine"
    "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
    "UAU" | "UAC" -> "Tyrosine"
    "UGU" | "UGC" -> "Cysteine"
    "UGG" -> "Tryptophan"
    _ -> ""
  }
}

fn not_stop_condon(x) {
  let stop = ["UAA", "UAG", "UGA"]
  !list.contains(stop, x)
}
