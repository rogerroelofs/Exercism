defmodule PrimeFactors do
  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest.
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(1), do: []
  def factors_for(number), do: factors(number, 2, []) |> Enum.reverse()

  @spec factors(pos_integer, pos_integer, [pos_integer]) :: [pos_integer]
  defp factors(n, div, acc) when rem(n, div) == 0,
    do: factors(div(n, div), div, [div | acc])

  defp factors(n, div, acc) when div < n, do: factors(n, div + 1, acc)
  defp factors(_, _, acc), do: acc
end
