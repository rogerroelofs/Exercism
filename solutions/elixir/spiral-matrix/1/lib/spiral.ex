defmodule Spiral do
  @doc """
  Given the dimension, return a square matrix of numbers in clockwise spiral order.
  """
  @spec matrix(dimension :: integer) :: list(list(integer))
  def matrix(dimension), do: spiral(1, dimension, dimension)

  defp spiral(_start, _row, 0), do: []

  defp spiral(start, row, column) do
    [generate(start, column) | order(start, column, row)]
  end

  defp generate(start, column) do
    start..(start + column - 1) |> Enum.to_list()
  end

  defp order(start, column, row) do
    spiral(start + column, column, row - 1) |> rotate
  end

  defp rotate(matrix), do: matrix |> Enum.reverse() |> transpose()

  defp transpose(matrix), do: matrix |> List.zip() |> Enum.map(&Tuple.to_list/1)
end
