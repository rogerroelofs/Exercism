defmodule ResistorColor do
  @doc """
  Return the value of a color band
  """
  @spec code(atom) :: integer()
  def code(color) when color == :black, do: 0
  def code(color) when color == :brown, do: 1
  def code(color) when color == :red, do: 2
  def code(color) when color == :orange, do: 3
  def code(color) when color == :yellow, do: 4
  def code(color) when color == :green, do: 5
  def code(color) when color == :blue, do: 6
  def code(color) when color == :violet, do: 7
  def code(color) when color == :grey, do: 8
  def code(color) when color == :white, do: 9
end
