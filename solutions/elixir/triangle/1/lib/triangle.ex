defmodule Triangle do
  @type kind :: :equilateral | :isosceles | :scalene

  @doc """
  Return the kind of triangle of a triangle with 'a', 'b' and 'c' as lengths.
  """
  @spec kind(number, number, number) :: {:ok, kind} | {:error, String.t()}
  def kind(a, b, c) when a > 0 and b > 0 and c > 0 do
    case Enum.sort([a, b, c]) do
      [x, y, z] when x + y <= z -> {:error, "side lengths violate triangle inequality"}
      [x, x, x] -> {:ok, :equilateral}
      [x, x, _] -> {:ok, :isosceles}
      [_, x, x] -> {:ok, :isosceles}
      [_, _, _] -> {:ok, :scalene}
    end
  end

  def kind(_, _, _), do: {:error, "all side lengths must be positive"}
end
