defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(0) do
    "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
  end

  def verse(number) do
    "#{bottles(number)} of beer on the wall, #{bottles(number)} of beer.\nTake #{pronoun(number)} down and pass it around, #{bottles(number - 1)} of beer on the wall.\n"
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    Enum.map(range, &verse(&1))
    |> Enum.join("\n")
  end

  @spec bottles(Integer.t()) :: String.t()
  def bottles(count) do
    case count do
      0 -> "no more bottles"
      1 -> "1 bottle"
      count -> "#{count} bottles"
    end
  end

  @spec pronoun(Integer.t()) :: String.t()
  def pronoun(1), do: "it"
  def pronoun(_), do: "one"
end
