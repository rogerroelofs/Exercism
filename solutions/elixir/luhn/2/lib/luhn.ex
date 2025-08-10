defmodule Luhn do
  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    number = String.replace(number, " ", "", trim: true)

    if String.length(number) < 2 || Regex.match?(~r/[[:^digit:]]/, number) do
      false
    else
      number
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
      |> Kernel.rem(10)
      |> Kernel.==(0)
    end
  end
end
