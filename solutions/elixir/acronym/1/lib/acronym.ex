defmodule Acronym do
  @moduledoc """
  generate acronym from phrase
  """
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    string
    |> String.split([" ", "-"]) # split into words on space or dash
    |> Enum.map(&String.replace(&1, ["_", "'", "-"], ""))
    |> Enum.filter(&(&1 != "")) # remove empty strings
    |> Enum.map(&String.at(&1, 0))
    |> Enum.map(&String.upcase/1)
    |> Enum.join()
  end
end
