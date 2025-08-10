defmodule Minesweeper do
  @doc """
  Annotate empty spots next to mines with the number of mines next to them.
  """
  @spec annotate([String.t()]) :: [String.t()]
  def annotate(board) do
    internal = board
    |> Enum.map(fn row -> row |> String.graphemes end)

    Enum.with_index(board)
    |> Enum.map(fn {row, row_index} ->
      Enum.with_index(row |> String.graphemes)
      |> Enum.map(fn {cell, cell_index} ->
        if cell == "*" do
          "*"
        else
          count_mines(internal, row_index, cell_index)
          |> number_to_string
        end
      end)
      |> Enum.join
    end)
  end

  @spec count_mines([[String.t()]], integer(), integer()) :: integer()
  defp count_mines(board, row, col) do
    Enum.reduce([-1, 0, 1], 0, fn row_offset, acc ->
      Enum.reduce([-1, 0, 1], acc, fn col_offset, acc ->
        if row + row_offset < 0 or row + row_offset >= length(board) do
          acc
        else
          if col + col_offset < 0 or col + col_offset >= length(Enum.at(board, row + row_offset)) do
            acc
          else
            if Enum.at(Enum.at(board, row + row_offset), col + col_offset) == "*" do
              acc + 1
            else
              acc
            end
          end
        end
      end)
    end)
  end

  defp number_to_string(0), do: " "
  defp number_to_string(number), do: Integer.to_string(number)
end
