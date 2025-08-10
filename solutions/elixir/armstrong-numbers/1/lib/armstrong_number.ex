defmodule ArmstrongNumber do
  @moduledoc """
  Provides a way to validate whether or not a number is an Armstrong number
  """
@doc """
valid? returns true if the argument is an Armstrong Number and false otherwise
"""
  @spec valid?(integer) :: boolean
  def valid?(number) when number == 0, do: true
  def valid?(number) do
    digits = number |> Integer.digits()
    power = digits |> length()
    number == digits |> Enum.reduce(0, fn (digit, acc) ->
      acc + Integer.pow(digit, power)
    end)
  end
end
