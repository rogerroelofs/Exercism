defmodule Isogram do
  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t()) :: boolean
  def isogram?(""), do: true

  def isogram?(sentence) do
    sentence
    |> String.replace(~r/[- ]/, "")
    |> String.downcase()
    |> String.graphemes()
    |> Enum.frequencies()
    |> Map.values()
    |> Enum.max() == 1
  end
end
