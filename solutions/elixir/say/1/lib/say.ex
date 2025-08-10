defmodule Say do
  @ones ~w(zero one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen)
  @tens ~w(zero ten twenty thirty forty fifty sixty seventy eighty ninety)
  @mag ~w(thousand million billion)

  @doc """
  Translate a positive integer into English.
  """
  @spec in_english(integer) :: {atom, String.t()}
  def in_english(number) do
    case number do
      0 ->
        {:ok, "zero"}

      num when num < 0 or num > 999_999_999_999 ->
        {:error, "number is out of range"}

      _ ->
        {
          :ok,
          to_string(number)
          |> String.reverse()
          |> String.graphemes()
          |> Enum.map(&Integer.parse/1)
          |> Enum.map(&elem(&1, 0))
          |> Enum.chunk_every(3)
          |> Enum.map(&convert_triplet/1)
          |> Enum.with_index()
          |> Enum.map(fn {triplet, index} ->
            if index > 0 and triplet > "" do
              "#{triplet} #{Enum.at(@mag, index - 1)}"
            else
              triplet
            end
          end)
          |> Enum.filter(&(&1 > ""))
          |> Enum.reverse()
          |> Enum.join(" ")
          #  |> IO.inspect()
        }
    end
  end

  defp convert_triplet([one]) when one == 0 do
    ""
  end

  defp convert_triplet([one]) do
    Enum.at(@ones, one)
  end

  defp convert_triplet([one, ten]) when ten == 0 do
    convert_triplet([one])
  end

  defp convert_triplet([one, ten]) when ten == 0 and one == 0 do
    ""
  end

  defp convert_triplet([one, ten]) when ten == 1 do
    Enum.at(@ones, 10 + one)
  end

  defp convert_triplet([one, ten]) when ten > 1 and one == 0 do
    "#{Enum.at(@tens, ten)}"
  end

  defp convert_triplet([one, ten]) when ten > 1 do
    "#{Enum.at(@tens, ten)}-#{Enum.at(@ones, one)}"
  end

  defp convert_triplet([one, ten, hundred]) when hundred == 0 do
    convert_triplet([one, ten])
  end

  defp convert_triplet([one, ten, hundred]) when ten == 0 and one == 0 do
    "#{Enum.at(@ones, hundred)} hundred"
  end

  defp convert_triplet([one, ten, hundred]) do
    "#{Enum.at(@ones, hundred)} hundred #{convert_triplet([one, ten])}"
  end
end
