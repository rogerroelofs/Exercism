defmodule PigLatin do
  @vowels ~w(a e i o u)
  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split(" ", trim: true)
    |> Enum.map(&xlate_word(&1) <> "ay")
    |> Enum.join(" ")
  end

  @spec xlate_word(word :: String.t()) :: String.t()
  defp xlate_word(word) do
    cond do
      String.starts_with?(word, @vowels) -> word
      String.starts_with?(word, ["x", "y"]) ->
        if Enum.member?(@vowels, String.slice(word, 1..1)) do
          shift_letter(word) |> xlate_word
        else
          word
        end
      String.starts_with?(word, "qu") ->
        shift_letter(word) |> shift_letter |> xlate_word
      true -> shift_letter(word) |> xlate_word
    end
  end

  defp shift_letter(word) do
    (String.slice(word, 1..-1) <> String.first(word))
  end
end
