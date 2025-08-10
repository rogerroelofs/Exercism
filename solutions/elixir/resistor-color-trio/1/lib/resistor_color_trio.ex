defmodule ResistorColorTrio do
  @colors ~w(black brown red orange yellow green blue violet grey white)a
  @doc """
  Calculate the resistance value in ohm or kiloohm from resistor colors
  """
  @spec label(colors :: [atom]) :: {number, :ohms | :kiloohms}
  def label(colors) do
    digits =
      colors
      |> Enum.take(3)
      |> Enum.map(fn color -> Enum.find_index(@colors, &(&1 == color)) end)

    [first, second | rest] = digits
    base = [first, second] |> Enum.join() |> String.to_integer()
    ret = base * 10 ** List.first(rest)

    if rem(ret, 1000) == 0 do
      {div(ret, 1000), :kiloohms}
    else
      {ret, :ohms}
    end
  end
end
