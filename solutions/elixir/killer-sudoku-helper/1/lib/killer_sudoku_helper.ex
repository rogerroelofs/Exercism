defmodule KillerSudokuHelper do
  @doc """
  Return the possible combinations of `size` distinct numbers from 1-9 excluding `exclude` that sum up to `sum`.
  """
  @spec combinations(cage :: %{exclude: [integer], size: integer, sum: integer}) :: [[integer]]
  def combinations(%{exclude: exclude, size: size, sum: sum}) do
    all_combos(Enum.to_list(1..9) -- exclude, size)
    |> Enum.filter(&(Enum.sum(&1) == sum))
  end

  defp all_combos(sorted, 1), do: Enum.map(sorted, &[&1])
  defp all_combos(sorted, size) do
    Enum.flat_map(sorted, fn x ->
      sorted
      |> Enum.filter(&(&1 > x))
      |> all_combos(size - 1)
      |> Enum.map(&[x | &1])
    end)
  end
end
