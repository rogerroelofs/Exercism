defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(<<char>> <> tail, shift) do
    <<encode(char, shift)>> <> rotate(tail, shift)
  end
  def rotate(_, _), do: ""

  defp encode(char, shift) when ((char >= ?a) and (char <= ?z)) do
    ?a + rem(char - ?a + shift, 26)
  end

  defp encode(char, shift) when ((char >= ?A) and (char <= ?Z)) do
    ?A + rem(char - ?A + shift, 26)
  end

  defp encode(char, _), do: char
end
