defmodule BottleSong do
  @moduledoc """
  Handles lyrics of the popular children song: Ten Green Bottles
  """

  @ordinals {
    "no",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten"
  }

  @spec recite(pos_integer, pos_integer) :: String.t()
  def recite(start_bottle, take_down) do
    Enum.map(start_bottle..(start_bottle - take_down + 1), fn bottle ->
      recite_verse(bottle)
    end)
    |> Enum.join("\n\n")
  end

  defp recite_verse(bottle) do
    start = String.capitalize(ordinal(bottle))
    stop = ordinal(bottle - 1)
    start_bottles = plural(bottle, "bottle")
    stop_bottles = plural(bottle - 1, "bottle")

    "#{start} green #{start_bottles} hanging on the wall,
#{start} green #{start_bottles} hanging on the wall,
And if one green bottle should accidentally fall,
There'll be #{stop} green #{stop_bottles} hanging on the wall."
  end

  defp plural(1, noun) do
    noun
  end

  defp plural(_, noun) do
    "#{noun}s"
  end

  defp ordinal(number) when number <= 10 do
    elem(@ordinals, number)
  end
end
