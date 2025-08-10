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
    length list
  end

  def exciting_list?(list) do
    interesting = for "Elixir" <- list, do: true
    (length interesting) > 0
  end
end
