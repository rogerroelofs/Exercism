defmodule Grains do
  @doc """
  Calculate two to the power of the input minus one.
  """
  @spec square(pos_integer()) :: {:ok, pos_integer()} | {:error, String.t()}
  def square(number) do
    exponent = number - 1

    if exponent >= 0 and exponent < 64 do
      {:ok, 2 ** exponent}
    else
      {:error, "The requested square must be between 1 and 64 (inclusive)"}
    end
  end

  @doc """
  Adds square of each number from 1 to 64.
  """
  @spec total :: {:ok, pos_integer()}
  def total do
    sum =
      Enum.map(1..64, fn n ->
        {:ok, square} = square(n)
        square
      end)
      |> Enum.sum()

    {:ok, sum}
  end
end
