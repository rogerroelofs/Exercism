defmodule Scrabble do
  @doc """
  Calculate the scrabble score for the word.
  """
  @spec score(String.t()) :: non_neg_integer
  def score(word) do
    word
    |> String.replace(~r/\W/, "", global: true)
    |> String.upcase
    |> String.to_charlist
    |> Enum.map(&score_letter/1)
    |> Enum.sum
  end

  defp score_letter(char) when char in ~c(A E I O U L N R S T), do: 1
  defp score_letter(char) when char in ~c(D G), do: 2
  defp score_letter(char) when char in ~c(B C M P), do: 3
  defp score_letter(char) when char in ~c(F H V W Y), do: 4
  defp score_letter(char) when char == ?K, do: 5
  defp score_letter(char) when char in ~c(J X), do: 8
  defp score_letter(char) when char in ~c(Q Z), do: 10
end
