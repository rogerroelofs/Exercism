defmodule FoodChain do
  @doc """
  Generate consecutive verses of the song 'I Know an Old Lady Who Swallowed a Fly'.
  """

  @animals [
    %{kind: "fly"},
    %{kind: "spider", verse: "It wriggled and jiggled and tickled inside her."},
    %{kind: "bird", verse: "How absurd to swallow a bird!"},
    %{kind: "cat", verse: "Imagine that, to swallow a cat!"},
    %{kind: "dog", verse: "What a hog, to swallow a dog!"},
    %{kind: "goat", verse: "Just opened her throat and swallowed a goat!"},
    %{kind: "cow", verse: "I don't know how she swallowed a cow!"},
    %{kind: "horse", verse: "She's dead, of course!"}
  ]

  @spec recite(start :: pos_integer, stop :: pos_integer) :: String.t()
  def recite(stop, stop) do
    (recite_verse(stop) |> Enum.join("\n")) <> "\n"
  end

  def recite(start, stop) do
    start..stop
    |> Enum.map(&recite(&1, &1))
    |> Enum.join("\n")
  end

  @spec recite_verse(index :: pos_integer) :: [String.t()]
  defp recite_verse(index) do
    animal = @animals |> Enum.at(index - 1)
    animal_verse = Map.get(animal, :verse)
    [start(animal.kind), animal_verse | verse(index - 1)] |> Enum.reject(&is_nil/1)
  end

  @spec verse(index :: non_neg_integer) :: [String.t()]
  defp verse(0) do
    ["I don't know why she swallowed the fly. Perhaps she'll die."]
  end

  defp verse(2) do
    [
      "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
      | verse(1)
    ]
  end

  defp verse(7) do
    []
  end

  defp verse(index) do
    current_animal = @animals |> Enum.at(index)
    previous_animal = @animals |> Enum.at(index - 1)
    [combine(current_animal.kind, previous_animal.kind) | verse(index - 1)]
  end

  defp start(animal_name) do
    "I know an old lady who swallowed a #{animal_name}."
  end

  defp combine(current_animal, previous_animal) do
    "She swallowed the #{current_animal} to catch the #{previous_animal}."
  end
end
