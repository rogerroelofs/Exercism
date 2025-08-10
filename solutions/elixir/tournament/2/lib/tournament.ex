defmodule Tournament do
  @doc """
  Given `input` lines representing two teams and whether the first of them won,
  lost, or reached a draw, separated by semicolons, calculate the statistics
  for each team's number of games played, won, drawn, lost, and total points
  for the season, and return a nicely-formatted string table.

  A win earns a team 3 points, a draw earns 1 point, and a loss earns nothing.

  Order the outcome by most total points for the season, and settle ties by
  listing the teams in alphabetical order.
  """

  @rules %{
    "win" => %{mp: 1, w: 1, p: 3},
    "loss" => %{mp: 1, l: 1},
    "draw" => %{mp: 1, d: 1, p: 1}
  }

  @spec tally(input :: list(String.t())) :: String.t()
  def tally(input) do
    output = input
    |> Enum.filter(&valid?/1)
    |> Enum.map(fn item -> String.split(item, ";") end)
    |> Enum.reduce(%{}, &calculate_stats/2)
    |> Map.to_list
    |> Enum.sort(fn({t1, %{p: a}}, {t2, %{p: b}}) ->
      cond do
        a == b -> t1 <= t2
        true -> a >= b
      end
    end)
    |> Enum.map(&format_output/1)
    |> Enum.join("\n")

    if output == "" do
      header()
    else
      ~s(#{header()}\n#{output})
    end
  end

  defp valid?(line) do
    parts = String.split(line, ";")
    if length(parts) == 3 do
      [_, _, outcome] = parts
      Enum.member?(["win", "loss", "draw"], outcome)
    else
      false
    end
  end

  defp calculate_stats([team1, team2, outcome], acc) do
    acc
    |> Map.put(team1, do_stats(acc, team1, 0, outcome))
    |> Map.put(team2, do_stats(acc, team2, 1, outcome))
  end

  defp do_stats(acc, team, position, outcome) do
    team
    |> get_stats(acc)
    |> update_stats(position, outcome)
  end

  defp get_stats(team, acc) do
    Map.get(acc, team, %{mp: 0, w: 0, l: 0, d: 0, p: 0})
  end

  defp update_stats(stats, 0, "win"), do: apply_rules(stats, "win")
  defp update_stats(stats, 1, "loss"), do: apply_rules(stats, "win")
  defp update_stats(stats, 0, "loss"), do: apply_rules(stats, "loss")
  defp update_stats(stats, 1, "win"), do: apply_rules(stats, "loss")
  defp update_stats(stats, _, "draw"), do: apply_rules(stats, "draw")

  defp apply_rules(stats, outcome) do
    Map.merge(stats, @rules[outcome], fn (_k, v1, v2) -> v1 + v2 end)
  end

  defp header() do
    """
    Team                           | MP |  W |  D |  L |  P
    """ |> String.trim
  end

  defp format_output({team, stats}) do
    scores = [:mp, :w, :d, :l, :p]
    |> Enum.map(fn key ->
      " | #{String.pad_leading(to_string(Map.get(stats, key)), 2)}"
    end)
    "#{String.pad_trailing(team, 30)}#{scores}"
  end
end
