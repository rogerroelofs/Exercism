defmodule Sublist do
  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  def compare(a, a), do: :equal

  def compare(a, b) do
    cond do
      contains?(b, a) -> :sublist
      contains?(a, b) -> :superlist
      true -> :unequal
    end
  end

  defp contains?([], _), do: false

  defp contains?([_ | t] = a, b) do
    List.starts_with?(a, b) or contains?(t, b)
  end
end
