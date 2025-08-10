defmodule CryptoSquare do
  @doc """
  Encode string square methods
  ## Examples

    iex> CryptoSquare.encode("abcd")
    "ac bd"
  """
  @spec encode(String.t()) :: String.t()
  def encode(""), do: ""
  def encode(str) do
    encoded = String.replace(str, ~r/[^[:alnum:]]/, "") |> String.downcase
    square = :math.sqrt(String.length(encoded)) |> ceil
    encoded
    |> String.graphemes
    |> Enum.chunk_every(square, square, Stream.cycle([" "]))
    |> List.zip
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.join(" ")
  end
end
