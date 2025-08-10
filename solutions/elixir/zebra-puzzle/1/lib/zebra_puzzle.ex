defmodule ZebraPuzzle do
  @moduledoc """
  Solve the zebra puzzle.
  """
  @nationalities [:englishman, :ukrainian, :spaniard, :norwegian, :japanese]
  @colors [:red, :green, :ivory, :yellow, :blue]
  @animals [:dog, :snails, :fox, :horse, :zebra]
  @drinks [:coffee, :tea, :milk, :juice, :water]
  @brands [:old_gold, :kools, :chesterfields, :lucky_strike, :parliaments]

@doc """
  Determine who drinks the water
  """
  @spec drinks_water() :: atom
  def drinks_water() do
    solve()
    |> Enum.find_value(fn
      {nation, _, _, :water, _} -> nation
      _ -> false
    end)
  end

  @doc """
  Determine who owns the zebra
  """
  @spec owns_zebra() :: atom
  def owns_zebra() do
    solve()
    |> Enum.find_value(fn
      {nation, _, :zebra, _, _} -> nation
      _ -> false
    end)
  end

  defp solve() do
    # filtering as we go lowers the number of permutations
    for nations <- permutations(@nationalities),
      order?(nations, :norwegian, 0) do                                   # 10
        for colors <- permutations(@colors),
          matches?(colors, :red, nations, :englishman),                   # 2
          leftOf?(colors, :ivory, colors, :green),                        # 6
          nextTo?(colors, :blue, nations, :norwegian) do                  # 15
            for animals <- permutations(@animals),
              matches?(animals, :dog, nations, :spaniard) do              # 3
                for drinks <- permutations(@drinks),
                  order?(drinks, :milk, 2),                               # 9
                  matches?(drinks, :coffee, colors, :green),              # 4
                  matches?(drinks, :tea, nations, :ukrainian) do          # 5
                    for brands <- permutations(@brands),
                      matches?(brands, :old_gold, animals, :snails),      # 7
                      matches?(brands, :kools, colors, :yellow),          # 8
                      matches?(brands, :parliaments, nations, :japanese), # 14
                      matches?(brands, :lucky_strike, drinks, :juice),    # 13
                      nextTo?(brands, :chesterfields, animals, :fox),     # 11
                      nextTo?(brands, :kools, animals, :horse) do         # 12
                        List.zip([nations, colors, animals, drinks, brands])
                    end
                end
            end
        end
    end
    |> List.flatten()
    |> Enum.reject(&(&1 == []))
  end

  defp order?(list, term, target) do
    Enum.find_index(list, &(&1 == term)) == target
  end

  defp matches?(from, t1, target, t2) do
    i = Enum.find_index(from, &(&1 == t1))
    Enum.at(target, i) == t2
  end

  defp leftOf?(from, t1, target, t2) do
    i = Enum.find_index(from, &(&1 == t1))
    Enum.at(target, i + 1) == t2
  end

  defp nextTo?(from, t1, target, t2) do
    i = Enum.find_index(from, &(&1 == t1))
    Enum.at(target, i + 1) == t2 || (i > 0 && Enum.at(target, i - 1) == t2)
  end

  defp permutations([]), do: [[]]
  defp permutations(list) do
    for elem <- list, rest <- permutations(list -- [elem]), do: [elem | rest]
  end
end
