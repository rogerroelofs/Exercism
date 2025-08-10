defmodule BoutiqueInventory do
  @spec sort_by_price(list) :: list
  def sort_by_price(inventory) do
    Enum.sort_by(inventory, &(&1.price))
  end

  def with_missing_price(inventory) do
    Enum.filter(inventory, &(&1.price == nil))
  end

  def increase_quantity(item, count) do
    %{quantity_by_size: quantity_by_size} = item
    qbs = quantity_by_size
      |> Enum.map(fn {k, v} ->
        {k, v + count}
      end)
      |> Enum.into(%{})
    Map.put(item, :quantity_by_size, qbs)
  end

  def total_quantity(item) do
    Enum.reduce(item.quantity_by_size, 0, fn({_k, v}, acc) -> v + acc end)
  end
end
