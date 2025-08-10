defmodule MatchingBrackets do
  @doc """
  Checks that all the brackets and braces in the string are matched correctly, and nested correctly
  """
  @spec check_brackets(String.t()) :: boolean
  def check_brackets(str) do
    str
    |> to_charlist()
    |> Enum.reduce_while([], &reducer/2) == []
  end

  @spec reducer(Char.t(), list) :: Tuple.t()
  defp reducer(b, acc) when b in '{([', do: {:cont, [b | acc]}
  defp reducer(?], [?[ | acc]), do: {:cont, acc}
  defp reducer(?), [?( | acc]), do: {:cont, acc}
  defp reducer(?}, [?{ | acc]), do: {:cont, acc}
  defp reducer(b, _) when b in '})]', do: {:halt, nil}
  defp reducer(_, acc), do: {:cont, acc}
end
