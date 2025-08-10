defmodule Knapsack do
  @doc """
  Return the maximum value that a knapsack can carry.
  """
  @spec maximum_value(items :: [%{value: integer, weight: integer}], maximum_weight :: integer) ::
          integer
  def maximum_value([], _), do: 0

  def maximum_value([%{value: value, weight: weight} | tail], maximum_weight) do
    cond do
      weight > maximum_weight ->
        maximum_value(tail, maximum_weight)

      true ->
        max(
          value + maximum_value(tail, maximum_weight - weight),
          maximum_value(tail, maximum_weight)
        )
    end
  end
end
