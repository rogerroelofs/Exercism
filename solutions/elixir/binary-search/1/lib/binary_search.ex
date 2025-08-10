defmodule BinarySearch do
  @doc """
    Searches for a key in the tuple using the binary search algorithm.
    It returns :not_found if the key is not in the tuple.
    Otherwise returns {:ok, index}.

    ## Examples

      iex> BinarySearch.search({}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 2)
      :not_found

      iex> BinarySearch.search({1, 3, 5}, 5)
      {:ok, 2}

  """

  @spec search(tuple, integer) :: {:ok, integer} | :not_found
  def search({}, _key), do: :not_found

  def search(numbers, key) do
    bsearch(numbers, key, 0, tuple_size(numbers) - 1)
  end

  defp bsearch(_, _, start_idx, end_idx) when start_idx > end_idx, do: :not_found

  defp bsearch(numbers, key, start_idx, end_idx) do
    i = start_idx + div(end_idx - start_idx, 2)

    case elem(numbers, i) do
      ^key -> {:ok, i}
      result when result > key -> bsearch(numbers, key, start_idx, i - 1)
      result when result < key -> bsearch(numbers, key, i + 1, end_idx)
    end
  end
end
