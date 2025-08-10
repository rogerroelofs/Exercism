defmodule PascalsTriangle do
  @doc """
  Calculates the rows of a pascal triangle
  with the given height
  """
  @spec rows(integer) :: [[integer]]
  def rows(num) do
    rows(num, [[1]])
  end

  defp rows(1, acc), do: Enum.reverse(acc)
  defp rows(num, [h | t]), do: rows(num - 1, [row(h, [1]), h | t])

  defp row([h], acc), do: [h | acc]

  defp row([h0, h1 | t], acc) do
    row([h1 | t], [h0 + h1 | acc])
  end
end
