defmodule BirdCount do
  def today([]), do: nil
  def today([head | _]), do: head

  def increment_day_count([]), do: [1]
  def increment_day_count([head | tail]) do
    rest = if length(tail) > 0 do
      tail
    else
      []
    end
    [head + 1 | rest]
  end

  def has_day_without_birds?(list) do
    0 in list
  end

  def total([]), do: 0
  def total([head | tail]) do
    head + total(tail)
  end

  def busy_days([]), do: 0
  def busy_days([head | tail]) do
    count = if head >= 5 do
      1
    else
      0
    end
    count + busy_days(tail)
    # Enum.filter(list, fn x -> x >= 5 end) |> length
  end
end
