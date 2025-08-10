defmodule OcrNumbers do
  @doc """
  Given a 3 x 4 grid of pipes, underscores, and spaces, determine which number is represented, or
  whether it is garbled.
  """

  @digits [
    [" _ ", "| |", "|_|", "   "], #0
    ["   ", "  |", "  |", "   "], #1
    [" _ ", " _|", "|_ ", "   "], #2
    [" _ ", " _|", " _|", "   "], #3
    ["   ", "|_|", "  |", "   "], #4
    [" _ ", "|_ ", " _|", "   "], #5
    [" _ ", "|_ ", "|_|", "   "], #6
    [" _ ", "  |", "  |", "   "], #7
    [" _ ", "|_|", "|_|", "   "], #8
    [" _ ", "|_|", " _|", "   "], #9
  ]

  @spec convert([String.t()]) :: {:ok, String.t()} | {:error, String.t()}
  def convert(input) do
    lines = input |> Enum.chunk_every(4) # in case it's a multiple of 4
    last = List.last(lines) # validate last line - yes, I'm being lazy
    good = last |> Enum.filter(fn line -> rem(String.length(line), 3) == 0 end)
    cond do
      length(last) != 4 -> {:error, "invalid line count"}
      length(good) != 4 -> {:error, "invalid column count"}
      true ->
        {:ok, lines
          |> Enum.map(&decode_line/1)
          |> Enum.join(",")
        }
    end
  end

  defp decode_line(line) do
    Enum.map(line, fn row ->
      row
      |> String.graphemes()
      |> Enum.chunk_every(3)
      |> Enum.map(&Enum.join/1)
    end)
    # transpose
    |> List.zip
    |> Enum.map(&Tuple.to_list/1)
    # decode each cell
    |> Enum.map(&convert_one/1)
    |> Enum.join()
  end

  defp convert_one(input) do
    index = Enum.find_index(@digits, fn elem -> elem == input end)
    cond do
      index == nil -> "?"
      index >= 0 -> Integer.to_string(index)
    end
  end
end
