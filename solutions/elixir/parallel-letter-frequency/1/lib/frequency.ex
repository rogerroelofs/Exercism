defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers \\ 1) do
    Task.async_stream(texts, &do_frequency/1, max_concurrency: workers)
    |> Enum.reduce(%{}, fn {:ok, counts}, acc ->
      Map.merge(acc, counts, fn _k, v1, v2 -> v1 + v2 end)
    end)
  end

  defp do_frequency text do
    text
    |> String.downcase
    |> String.graphemes
    |> Enum.filter(fn char ->
      String.match?(char, ~r/\p{L}/)
    end)
    |> Enum.frequencies()
  end
end
