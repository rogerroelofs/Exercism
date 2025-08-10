defmodule BoutiqueInventory do
  @spec sort_by_price(list) :: list
  def sort_by_price(inventory) do
    Enum.sort_by(inventory, &(&1.price))
  end

  @spec with_missing_price(list) :: list
  def with_missing_price(inventory) do
    Enum.filter(inventory, &(&1.price == nil))
  end

  @spec increase_quantity(map, integer) :: map
  def increase_quantity(item, count) do
    Map.put(item, :quantity_by_size, item.quantity_by_size
    |> Enum.map(fn {k, v} -> {k, v + count} end)
    |> Enum.into(%{}))
  end

  @spec total_quantity(map) :: integer
  def total_quantity(item) do
    Enum.reduce(item.quantity_by_size, 0, fn({_k, v}, acc) -> v + acc end)
  end
end
