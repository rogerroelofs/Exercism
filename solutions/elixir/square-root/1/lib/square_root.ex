defmodule SquareRoot do
  @doc """
  Calculate the integer square root of a positive integer
  """
  @spec calculate(radicand :: pos_integer) :: pos_integer
  def calculate(1), do: 1
  def calculate(radicand) do
    sqrt(radicand / 2, radicand)
  end

  defp sqrt(num, radicand) do
    res = (num + radicand/num)/2
    if res * res == radicand do
      res
    else
      sqrt(res, radicand)
     end
   end
end
