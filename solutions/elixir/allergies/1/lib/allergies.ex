defmodule Allergies do
  use Bitwise, only_operators: true

  @allergies %{
    0b00000001 => "eggs",
    0b00000010 => "peanuts",
    0b00000100 => "shellfish",
    0b00001000 => "strawberries",
    0b00010000 => "tomatoes",
    0b00100000 => "chocolate",
    0b01000000 => "pollen",
    0b10000000 => "cats",
  }
  @doc """
  List the allergies for which the corresponding flag bit is true.
  """
  @spec list(non_neg_integer) :: [String.t()]
  def list(flags) do
    @allergies
    |> Enum.map(&(if (elem(&1, 0) &&& flags) > 0, do: elem(&1, 1) ))
    |> Enum.filter(&(&1 != nil))
  end

  @doc """
  Returns whether the corresponding flag bit in 'flags' is set for the item.
  """
  @spec allergic_to?(non_neg_integer, String.t()) :: boolean
  def allergic_to?(flags, item) do
    item in list(flags)
  end
end
