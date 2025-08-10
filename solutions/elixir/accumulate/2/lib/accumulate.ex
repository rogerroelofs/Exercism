defmodule Accumulate do
  @moduledoc """
  applies a function to the given list - can't use stdlib
  """
  @doc """
    Given a list and a function, apply the function to each list item and
    replace it with the function's return value.

    Returns a list.

    ## Examples

      iex> Accumulate.accumulate([], fn(x) -> x * 2 end)
      []

      iex> Accumulate.accumulate([1, 2, 3], fn(x) -> x * 2 end)
      [2, 4, 6]

  """

  @spec accumulate(list, (any -> any)) :: list
  def accumulate(list, fun) do
    accum(list, fun, [])
  end

  defp accum([head | tail], fun, acc) do
    [fun.(head) | accum(tail, fun, acc)]
  end
  defp accum([], _fun, acc) do
    acc
  end
end
