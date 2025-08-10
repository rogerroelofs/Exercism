defmodule Atbash do
  @plain 'abcdefghijklmnopqrstuvwxyz'
  @cipher Enum.reverse(@plain)

  @doc """
  Encode a given plaintext to the corresponding ciphertext

  ## Examples

  iex> Atbash.encode("completely insecure")
  "xlnko vgvob rmhvx fiv"
  """
  @spec encode(String.t()) :: String.t()
  def encode(plaintext) do
    plaintext
    |> String.downcase()
    |> String.replace(~r"[\W]", "")
    |> to_charlist()
    |> Enum.map(&swapper(&1, @plain, @cipher))
    |> Enum.chunk_every(5)
    |> Enum.join(" ")
  end

  @spec decode(String.t()) :: String.t()
  def decode(cipher) do
    cipher
    |> String.replace(" ", "")
    |> to_charlist()
    |> Enum.map(&swapper(&1, @cipher, @plain))
    |> to_string
  end

  defp swapper(char, from, to) do
    i = Enum.find_index(from, &(&1 == char))
    if is_nil(i), do: char, else: to |> Enum.at(i)
  end
end
