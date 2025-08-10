defmodule Dominoes do
  @type domino :: {1..6, 1..6}

  @doc """
  chain?/1 takes a list of domino stones and returns boolean indicating if it's
  possible to make a full chain
  """
  @spec chain?(dominoes :: [domino]) :: boolean
  def chain?([]), do: true
  def chain?([{same, same}]), do: true
  def chain?([{_, _}]), do: false
  def chain?([{a, b} | dominoes]) do
    dominoes
    |> Enum.any?(
      fn
        {x, ^a} = domino -> chain?([{b, x} | List.delete(dominoes, domino)])
        {^a, x} = domino -> chain?([{b, x} | List.delete(dominoes, domino)])
        _ -> false
      end
    )
  end
end
