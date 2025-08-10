defmodule AllYourBase do
  @moduledoc """
  this module converts from one base to another
  """
  @doc """
  Given a number in input base, represented as a sequence of digits, converts it to output base,
  or returns an error tuple if either of the bases are less than 2
  """

  @spec convert(list, integer, integer) :: {:ok, list} | {:error, String.t()}
  def convert(_, _, output_base) when output_base < 2, do: {:error, "output base must be >= 2"}
  def convert(_, input_base, _) when input_base < 2, do: {:error, "input base must be >= 2"}
  def convert([], _input_base, _output_base), do: {:ok, [0]}
  def convert(digits, input_base, output_base) do
    if digits |> Enum.any?(&(&1 < 0 or &1 >= input_base)) do
      {:error, "all digits must be >= 0 and < input base"}
    else
      digits =
        digits
        |> to_int(input_base)
        |> from_int(output_base)

      {:ok, digits}
    end
  end

  @spec to_int(list, integer) :: integer
  defp to_int(digits, base) do
    Enum.reduce(digits, 0, fn digit, acc ->
      acc * base + digit
    end)
  end

  @spec from_int(integer, integer, list) :: list
  defp from_int(number, base, digits \\ [])
  defp from_int(0, _base, []), do: [0]
  defp from_int(0, _base, digits), do: digits
  defp from_int(number, base, digits) do
    digit = rem(number, base)
    number = trunc(number / base)
    from_int(number, base, [digit | digits])
  end
end
