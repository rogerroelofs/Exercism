defmodule Prime do
  @doc """
  Generates the nth prime.
  """
  @spec nth(pos_integer) :: non_neg_integer
  def nth(count) when count > 0 do
    Stream.iterate(2, &next_prime/1) |> Enum.at(count-1)
  end

  defp prime?(num) do
    last = num
      |> :math.sqrt
      |> Float.ceil
      |> trunc
    notprime = 2..last
      |> Enum.any?(fn a -> rem(num, a)==0 end)
    !notprime
  end

  defp next_prime(num) do
    cond do
      prime?(num + 1) -> num + 1
      true -> next_prime(num + 1)
    end
  end
end
