defmodule Proverb do
  @doc """
  Generate a proverb from a list of strings.
  """

  @spec recite(strings :: [String.t()]) :: String.t()
  def recite([]), do: ""
  def recite([string]), do: "And all for the want of a #{string}.\n"

  def recite([first | rest] = strings) do
    lines =
      Enum.zip(strings, rest)
      |> Enum.map_join("\n", fn {prev, cur} -> "For want of a #{prev} the #{cur} was lost." end)

    String.trim_leading("""
    #{lines}
    And all for the want of a #{first}.
    """)
  end
end
