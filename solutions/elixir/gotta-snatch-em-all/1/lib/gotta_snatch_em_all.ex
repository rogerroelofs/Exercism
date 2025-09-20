defmodule GottaSnatchEmAll do
  @type card :: String.t()
  @type collection :: MapSet.t(card())

  @spec new_collection(card()) :: collection()
  def new_collection(card), do: MapSet.new([card])

  @spec add_card(card(), collection()) :: {boolean(), collection()}
  def add_card(card, collection) do
    if MapSet.member?(collection, card) do
      {true, collection}
    else
      {false, MapSet.put(collection, card)}
    end
  end

  @spec trade_card(card(), card(), collection()) :: {boolean(), collection()}
  def trade_card(your_card, their_card, collection) do
    can_trade = MapSet.member?(collection, your_card) && !MapSet.member?(collection, their_card)
    new_collection = collection |> MapSet.delete(your_card) |> MapSet.put(their_card)
    if can_trade do
      {true, new_collection}
    else
      {false, new_collection}
    end
  end

  @spec remove_duplicates([card()]) :: [card()]
  def remove_duplicates(cards) do
    cards
    |> MapSet.new()
    |> MapSet.to_list()
  end

  @spec extra_cards(collection(), collection()) :: non_neg_integer()
  def extra_cards(your_collection, their_collection) do
    MapSet.size(MapSet.difference(your_collection, their_collection))
  end

  @spec boring_cards([collection()]) :: [card()]
  def boring_cards(collections) do
    if Enum.empty?(collections), 
      do: [], 
      else: MapSet.to_list(Enum.reduce(collections, fn collection, acc -> MapSet.intersection(acc, collection) end))
  end

  @spec total_cards([collection()]) :: non_neg_integer()
  def total_cards(collections) do
    collections
    |> Enum.reduce(MapSet.new(), fn collection, acc -> MapSet.union(acc, collection) end)
    |> MapSet.size()
  end

  @spec split_shiny_cards(collection()) :: {[card()], [card()]}
  def split_shiny_cards(collection) do
    {shiny_cards, regular_cards} = MapSet.split_with(collection, fn card -> String.starts_with?(card, "Shiny") end)
    {MapSet.to_list(shiny_cards), MapSet.to_list(regular_cards)}
  end
end
