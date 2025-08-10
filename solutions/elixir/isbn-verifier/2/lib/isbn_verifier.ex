defmodule IsbnVerifier do
  @doc """
    Checks if a string is a valid ISBN-10 identifier

    ## Examples

      iex> IsbnVerifier.isbn?("3-598-21507-X")
      true

      iex> IsbnVerifier.isbn?("3-598-2K507-0")
      false

  """
  @spec isbn?(String.t()) :: boolean
  def isbn?(isbn) when is_binary(isbn) do
    isbn = String.replace(isbn, "-", "")

    unless String.match?(isbn, ~r/^\d{9}[X\d]$/) do
      false
    else
      isbn = String.replace(isbn, ~r/[^0-9X]/, "")

      isbn
      |> to_charlist()
      |> Enum.zip(10..1)
      |> Enum.map(fn {digit, index} ->
        digit = if digit == ?X, do: 10, else: digit - ?0
        digit * index
      end)
      |> Enum.sum()
      |> rem(11) == 0
    end
  end
end
