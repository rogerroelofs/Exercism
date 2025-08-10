defmodule Yacht do
  @type category ::
          :ones
          | :twos
          | :threes
          | :fours
          | :fives
          | :sixes
          | :full_house
          | :four_of_a_kind
          | :little_straight
          | :big_straight
          | :choice
          | :yacht

  @doc """
  Calculate the score of 5 dice using the given category's scoring method.
  """
  @spec score(category :: category(), dice :: [integer]) :: integer
  def score(:choice, dice), do: Enum.sum(dice)
  def score(:yacht, [first | rest]) do
    count = Enum.filter(rest, &(&1 == first)) |> Enum.count
    if count == 4, do: 50, else: 0
  end
  def score(:full_house, dice) do
    counts = count_kinds(dice)
    if find_counts(counts, 3) == 1 and
    find_counts(counts, 2) == 1, do: Enum.sum(dice), else: 0
  end
  def score(:four_of_a_kind, dice) do
    kind = count_kinds(dice)
    |> Enum.filter(fn {_, v} -> v in [4, 5] end)
    |> List.first
    if is_nil(kind), do: 0, else: elem(kind, 0) * 4
  end
  def score(category, dice) do
    case category do
      :ones -> score_same(dice, 1)
      :twos -> score_same(dice, 2)
      :threes -> score_same(dice, 3)
      :fours -> score_same(dice, 4)
      :fives -> score_same(dice, 5)
      :sixes -> score_same(dice, 6)
      :big_straight -> score_straight(dice, 6, 1)
      :little_straight -> score_straight(dice, 1, 6)
    end
  end

  defp score_same(dice, kind) do
    Enum.filter(dice, &(&1 == kind)) |> Enum.count |> Kernel.*(kind)
  end

  defp score_straight(dice, req_face, neg_face) do
    counts = count_kinds(dice)
    if find_counts(counts, 1) == 5 and
    has_face(counts, req_face) and
    not has_face(counts, neg_face) do
      30
    else
      0
    end
  end

  defp count_kinds(dice) do
    Enum.reduce(dice, %{}, fn(el, acc) ->
      Map.update(acc, el, 1, &(&1 + 1))
    end)
  end
  
  defp find_counts(counts, find_count) do
    counts
    |> Enum.filter(fn {_, v} -> v == find_count end)
    |> Enum.count
  end

  defp has_face(counts, req_face) do
    counts
    |> Enum.filter(fn {k, _} -> k == req_face end)
    |> List.first
    |> is_nil
    |> Kernel.not
  end  
end
