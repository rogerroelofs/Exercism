defmodule House do
  @doc """
  Return verses of the nursery rhyme 'This is the House that Jack Built'.
  """
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
    Enum.map_join(start..stop, "", fn i ->
      "This is #{do_verses(i)}"
    end)
  end

  defp do_verses(top) do
    @verses
    |> Enum.take(top)
    |> Enum.reverse()
    |> Enum.join(" ")
    |> Kernel.<>("\n")
  end
end
