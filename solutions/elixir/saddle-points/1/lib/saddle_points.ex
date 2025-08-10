defmodule SaddlePoints do
  @doc """
  Parses a string representation of a matrix
  to a list of rows
  """
  @spec rows(String.t()) :: [[integer]]
  def rows(""), do: []
  def rows(str) do
    String.split(str, "\n")
    |> Enum.map(fn line ->
      String.split(line, " ")
      |> Enum.map(&String.to_integer(&1))
    end)
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(""), do: []
  def columns(str) do
    rows(str)
    |> List.zip
    |> Enum.map(&Tuple.to_list/1)
  end

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(str) do
    maxes = str |> rows |> Enum.map(&Enum.max/1) |> Enum.with_index(1)
    mins = str |> columns |> Enum.map(&Enum.min/1) |> Enum.with_index(1)
    for {min, row} <- maxes, {max, col} <- mins, min == max, do: {row, col}
  end
end
