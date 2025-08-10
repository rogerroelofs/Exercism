defmodule Sieve do
  @doc """
  Generates a list of primes up to a given limit.
  """
  @spec primes_to(non_neg_integer) :: [non_neg_integer]
  def primes_to(limit) when limit < 2, do: []
  def primes_to(limit) do
    Enum.to_list(2..limit)
    |> primes
  end

  defp primes([p]), do: [p]
  defp primes(numbers) do
    [p | tail] = numbers
    multiples = Enum.map(numbers, fn n -> n*p end)
    filtered_tail = Enum.filter(tail, fn n -> !Enum.member?(multiples, n) end)
    [p | primes(filtered_tail)]
  end
end
