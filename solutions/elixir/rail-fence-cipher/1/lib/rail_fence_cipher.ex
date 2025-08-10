defmodule RailFenceCipher do
  @doc """
  Encode a given plaintext to the corresponding rail fence ciphertext
  """
  @spec encode(String.t(), pos_integer) :: String.t()
  def encode("", _), do: ""
  def encode(str, 1), do: str
  def encode(str, rails) do
    str
    |> String.graphemes
    |> zig_zag(rails)
    |> Enum.join
  end

  @doc """
  Decode a given rail fence ciphertext to the corresponding plaintext
  """
  @spec decode(String.t(), pos_integer) :: String.t()
  def decode("", _), do: ""
  def decode(str, 1), do: str
  def decode(str, rails) do
    zig_zag(0..String.length(str) - 1, rails)
    |> Enum.zip(String.graphemes(str))
    |> Enum.sort
    |> Enum.map(fn {_, v} -> v end)
    |> Enum.join
  end

  defp zig_zag(enum, rails) do
    cycle = 1..rails - 1 |> Enum.concat(rails..2) |> Stream.cycle
    for i <- 1..rails, {x, ^i} <- Enum.zip(enum, cycle), do: x
  end
end
