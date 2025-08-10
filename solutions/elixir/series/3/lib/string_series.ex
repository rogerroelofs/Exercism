defmodule StringSeries do
  @doc """
  Given a string `s` and a positive integer `size`, return all substrings
  of that size. If `size` is greater than the length of `s`, or less than 1,
  return an empty list.
  """
  @spec slices(s :: String.t(), size :: integer) :: list(String.t())
  def slices(_, size) when size <= 0, do: []
  def slices(s, size) do
    (for n <- 0..String.length(s) - size, do: String.slice(s, n, size))
    |> Enum.filter(&(String.length(&1) == size))
  end
end
