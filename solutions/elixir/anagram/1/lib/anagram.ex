defmodule Anagram do
  @moduledoc """
  given a base word and list of candidates, return anagrams
  """
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    candidates |> Enum.filter(&(check_candidate(base, &1)))
  end

  defp check_candidate(base, candidate) do
    cond do
      clean(base) == clean(candidate) -> false
      true -> normalize(base) == normalize(candidate)
    end
  end

  defp normalize(word) do
    word |> clean()
    |> String.split("", trim: true)
    |> Enum.sort()
    |> Enum.join()
  end

  defp clean(word) do
    word
    |> String.replace(~r"[\W\d]", "")
    |> String.downcase()
  end
end
