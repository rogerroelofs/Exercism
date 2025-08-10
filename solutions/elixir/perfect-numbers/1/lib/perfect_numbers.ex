defmodule PerfectNumbers do
  @doc """
  Determine the aliquot sum of the given `number`, by summing all the factors
  of `number`, aside from `number` itself.

  Based on this sum, classify the number as:

  :perfect if the aliquot sum is equal to `number`
  :abundant if the aliquot sum is greater than `number`
  :deficient if the aliquot sum is less than `number`
  """
  @spec classify(number :: integer) :: {:ok, atom} | {:error, String.t()}
  def classify(number) when number <= 0,
    do: {:error, "Classification is only possible for natural numbers."}

  def classify(number) do
    sum =
      factor(number)
      # remove last element and sum rest
      |> Enum.reverse()
      |> tl
      |> Enum.sum()

    cond do
      sum == number -> {:ok, :perfect}
      sum < number -> {:ok, :deficient}
      sum > number -> {:ok, :abundant}
    end
  end

  defp factor(1), do: [1]

  defp factor(n) do
    for(i <- 1..div(n, 2), rem(n, i) == 0, do: i) ++ [n]
  end
end
