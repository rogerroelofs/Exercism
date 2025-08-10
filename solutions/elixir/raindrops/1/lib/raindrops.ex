defmodule Raindrops do
  @sounds %{
    3 => "Pling",
    5 => "Plang",
    7 => "Plong"
  }
  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
    just pass the number's digits straight through.
  """
  @spec convert(pos_integer) :: String.t()
  def convert(number) do
    sounds = @sounds
    |> Enum.map(fn({factor, sound}) ->
      if rem(number, factor) == 0, do: sound, else: ""
    end)
    |> Enum.join("")
    case sounds do
      "" -> Integer.to_string(number)
      _ -> sounds
    end
  end
end
