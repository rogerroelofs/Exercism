defmodule Series do
  @doc """
  Finds the largest product of a given number of consecutive numbers in a given string of numbers.
  """
  @spec largest_product(String.t(), non_neg_integer) :: non_neg_integer
  def largest_product(_, 0), do: 1
  def largest_product("", _), do: raise ArgumentError
  def largest_product(_, size) when size < 0, do: raise ArgumentError

  def largest_product(number_string, size) do
    if String.length(number_string) < size, do: raise ArgumentError
    if String.match?(number_string, ~r/\D/), do: raise ArgumentError

    number_string
    |> String.to_charlist
    |> Enum.chunk_every(size, 1, :discard)
    |> Enum.map(fn digits ->
      digits
      |> Enum.map(&(&1 - 48))
      |> Enum.product
    end)
    |> Enum.sort
    |> List.last
  end
end
