defmodule TwelveDays do

  @ordinals [
    "",
    "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eighth",
    "ninth",
    "tenth",
    "eleventh",
    "twelfth",
  ]

  @gifts [
    "",
    "a Partridge in a Pear Tree",
    "two Turtle Doves",
    "three French Hens",
    "four Calling Birds",
    "five Gold Rings",
    "six Geese-a-Laying",
    "seven Swans-a-Swimming",
    "eight Maids-a-Milking",
    "nine Ladies Dancing",
    "ten Lords-a-Leaping",
    "eleven Pipers Piping",
    "twelve Drummers Drumming"
  ]

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """

  @spec verse(number :: integer) :: String.t()
  def verse(1), do: "#{verse_prefix(1)} #{Enum.slice(@gifts, 1..1)}."
  def verse(number) do
    [first | rest] = Enum.slice(@gifts, 1..number)
    "#{verse_prefix(number)} #{rest |> Enum.reverse |> Enum.join(", ")}, and #{first}."
  end

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
    Enum.map_join(starting_verse..ending_verse, "\n", &verse/1)
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing() :: String.t()
  def sing do
    verses(1, 12)
  end

  @spec verse_prefix(number :: integer()) :: String.t()
  defp verse_prefix(number) do
    "On the #{Enum.at(@ordinals, number)} day of Christmas my true love gave to me:"
  end
end
