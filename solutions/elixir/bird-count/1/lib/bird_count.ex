defmodule BirdCount do
  def today(list) do
    List.first list
  end

  def increment_day_count(list) do
    head = List.first(list, 0) + 1
    rest = if length(list) > 0 do
      tl(list)
    else
      []
    end
    [head | rest]
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
