defmodule ResistorColorDuo do
  @colors ~w(black brown red orange yellow green blue violet grey white)a
  @doc """
  Calculate a resistance value from two colors
  """
  @spec value(colors :: [atom]) :: integer
  def value(colors) do
    colors
    |> Enum.take(2)
    |> Enum.map(fn color -> Enum.find_index(@colors, &(&1 == color)) end)
    |> Enum.join
    |> String.to_integer()
  end
end
