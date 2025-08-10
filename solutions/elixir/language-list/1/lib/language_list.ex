defmodule LanguageList do
  def new() do
    []
  end

  def add(list, language) do
    [language | list]
  end

  def remove(list) do
    [_language | list] = list
    list
  end

  def first(list) do
    [first | _list] = list
    first
  end

  def count(list) do
    Enum.count list
  end

  def exciting_list?(list) do
    Enum.find(list, &(match?("Elixir", &1)))
  end
end
