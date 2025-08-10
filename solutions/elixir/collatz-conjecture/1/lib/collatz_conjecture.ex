defmodule CollatzConjecture do
  @doc """
  calc/1 takes an integer and returns the number of steps required to get the
  number to 1 when following the rules:
    - if number is odd, multiply with 3 and add 1
    - if number is even, divide by 2
  """
  @spec calc(input :: pos_integer()) :: non_neg_integer()
  def calc(input) when is_integer(input) and input > 0 do
    calc_steps(input, 0)
  end

  defp calc_steps(1, acc), do: acc
  defp calc_steps(input, acc) when rem(input, 2) != 0 do # is_odd
    calc_steps(input * 3 + 1, acc + 1)
  end
  defp calc_steps(input, acc) do
    calc_steps(div(input, 2), acc + 1)
  end
end
