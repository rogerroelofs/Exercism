defmodule Luhn do
  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?("0"), do: false
  def valid?(" 0"), do: false
  def valid?(number) do
    number = String.replace(number, " ", "")
    if String.match?(number, ~r/^[[:digit:]]+$/) do
      luhn = number
      |> String.reverse()
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
      |> Enum.with_index()
      |> Enum.map(fn {digit, i} ->
        if rem(i, 2) == 1 do
          digit = digit * 2
          if digit > 9, do: digit - 9, else: digit
        else
          digit
        end
      end)
      |> Enum.sum()

      rem(luhn, 10) == 0
    else
      false
    end
  end
end
