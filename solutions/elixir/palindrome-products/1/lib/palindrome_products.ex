defmodule PalindromeProducts do
  @doc """
  Generates all palindrome products from an optionally given min factor (or 1) to a given max factor.
  """
  @spec generate(non_neg_integer, non_neg_integer) :: map
  def generate(max_factor, min_factor \\ 1)
  def generate(max_factor, min_factor) when min_factor > max_factor do
    raise ArgumentError
  end
  def generate(max_factor, min_factor) do
    palindromes(min_factor, max_factor)
    |> Enum.group_by(fn {product, _} -> product end, fn {_, factors} -> factors end)
  end

  defp palindromes(min, max) do
    for i <- min..max,
        j <- i..max,
        product = i * j,
        palindrome?(product),
        do: {product, [i, j]}
  end

  defp palindrome?(int) do
    digits = to_string(int)
    digits == String.reverse(digits)
  end
end
