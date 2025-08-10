defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    factors = Enum.reject(factors, &(&1 === 0))

    1..(limit - 1)
    |> Enum.filter(fn item ->
      factors
      |> Enum.map(&rem(item, &1))
      |> Enum.any?(&(&1 === 0))
    end)
    |> Enum.sum()
  end
end
