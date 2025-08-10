defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """
  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def clean(raw) do
    if String.match?(raw, ~r/[^0-9\.+\- \(\)]/) do
      {:error, "must contain digits only"}
    else
      cleaned = raw
      |> String.replace(~r/\D/, "")
      cleaned = if String.match?(cleaned, ~r/^1\d{10}/) do
        String.slice(cleaned, 1..-1)
      else
        cleaned
      end
      cond do
        String.length(cleaned) == 11 ->
          {:error, "11 digits must start with 1"}
        String.match?(cleaned, ~r/0\d{9}/) ->
          {:error, "area code cannot start with zero"}
        String.length(cleaned) != 10 ->
          {:error, "incorrect number of digits"}
        String.match?(cleaned, ~r/1\d{9}/) ->
          {:error, "area code cannot start with one"}
        String.match?(cleaned, ~r/[2-9]\d\d0\d{6}/) ->
          {:error, "exchange code cannot start with zero"}
        String.match?(cleaned, ~r/[2-9]\d\d1\d{6}/) ->
          {:error, "exchange code cannot start with one"}
        String.match?(cleaned, ~r/[2-9]\d\d[2-9]\d{6}/) ->
          {:ok, cleaned}
        true ->
        {:error, "incorrect number of digits"}
      end
    end
  end
end
