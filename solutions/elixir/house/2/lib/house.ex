defmodule House do
  @doc """
  Return verses of the nursery rhyme 'This is the House that Jack Built'.
  """
  @preamble "This is "
  @postamble ""
  @verses """
          the house that Jack built.
          the malt that lay in
          the rat that ate
          the cat that killed
          the dog that worried
          the cow with the crumpled horn that tossed
          the maiden all forlorn that milked
          the man all tattered and torn that kissed
          the priest all shaven and shorn that married
          the rooster that crowed in the morn that woke
          the farmer sowing his corn that kept
          the horse and the hound and the horn that belonged to
          """
          |> String.split("\n")

  @spec recite(start :: integer, stop :: integer) :: String.t()
  def recite(start, stop) do
    Enum.map(start..stop, fn i ->
      "#{@preamble}#{do_verses(i)}#{@postamble}"
    end)
    |> Enum.join("\n")
    |> Kernel.<>("\n")
  end

  defp do_verses(top) do
    Enum.map(top..1, fn i ->
      if i > 0, do: Enum.at(@verses, i - 1), else: ""
    end)
    |> Enum.join(" ")
  end
end
