defmodule BookStore do
  @typedoc "A book is represented by its number in the 5-book series"
  @type book :: 1 | 2 | 3 | 4 | 5
  @prices {0, 800, 760, 720, 640, 600}

  defguardp in_prices(i) when i >= 0 and i < tuple_size(@prices)

  @doc """
  Calculate lowest price (in cents) for a shopping basket containing books.
  """
  @spec total(basket :: [book]) :: integer
  def total(basket) do
    basket
    |> Enum.frequencies()
    |> Enum.sort_by(&elem(&1, 1), :desc)
    |> calc_total()
  end

  @spec calc_total(List.t(), Integer.t()) :: Integer.t()
  defp calc_total(book_counts, sum \\ 0)
  defp calc_total([], sum), do: sum
  defp calc_total(book_counts, sum) do
    5..1
    |> Enum.map(fn
      i when i <= length(book_counts) ->
        next = discounted_price(i) + sum
        book_counts
        |> drop_discounted(i)
        |> calc_total(next)
      _ -> nil
    end)
    |> Enum.reject(&Kernel.is_nil/1)
    |> Enum.min()
  end

  @spec discounted_price(Integer.t()) :: Integer.t()
  defp discounted_price(count) when in_prices(count), do: count * elem(@prices, count)


  @spec drop_discounted(List.t(), Integer.t()) :: List.t()
  defp drop_discounted(book_counts, n_to_drop) do
    book_counts
    |> Enum.with_index()
    |> Enum.map(fn
      {{book, count}, i} when i < n_to_drop -> {book, count - 1}
      {book_count, _} -> book_count
    end)
    |> Enum.reject(fn {_, count} -> count == 0 end)
  end
end
