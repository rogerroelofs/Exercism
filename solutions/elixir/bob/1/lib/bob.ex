defmodule Bob do
  @response %{
    whatever: "Whatever.",
    chill: "Whoa, chill out!",
    sure: "Sure.",
    calm: "Calm down, I know what I\'m doing!",
    fine: "Fine. Be that way!"
  }

  @spec hey(String.t()) :: String.t()
  def hey(input) do
    cleaned =
      input
      |> String.trim()

    cond do
      String.length(cleaned) == 0 -> @response.fine
      question?(cleaned) and shouting?(cleaned) -> @response.calm
      question?(cleaned) -> @response.sure
      shouting?(cleaned) -> @response.chill
      true -> @response.whatever
    end
  end

  defp question?(input) do
    String.ends_with?(input, "?")
  end

  def shouting?(input) do
    String.upcase(input) == input &&
      String.match?(input, ~r/[[:alpha:]]/ui)
  end
end
