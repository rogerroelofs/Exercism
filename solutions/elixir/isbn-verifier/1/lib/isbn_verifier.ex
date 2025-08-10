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
    start_index = 10
    isbn = String.replace(isbn, "-", "")

    if String.length(isbn) == 10 do
      isbn = String.replace(isbn, ~r/[^0-9X]/, "")

      isbn
      |> to_charlist()
      |> Enum.with_index()
      |> Enum.map(fn {digit, index} ->
        digit = if digit == ?X, do: 10, else: digit - ?0
        digit * (start_index - index)
      end)
      |> Enum.sum()
      |> rem(11) == 0
    else
      false
    end
  end
end
