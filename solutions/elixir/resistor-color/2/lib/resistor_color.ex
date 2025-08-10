defmodule ResistorColor do
  @colors ~w(black brown red orange yellow green blue violet grey white)a

  @doc """
  Return the value of a color band
  """
  @spec code(atom) :: integer()
  def code(color) when color in @colors do
    Enum.find_index(@colors, &(&1 == color))
  end
  def code(color) do
    raise ArgumentError, message: "Invalid resistor color band: #{color}"
  end
end
