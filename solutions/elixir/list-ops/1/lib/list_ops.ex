defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l) do
    count(l, 0)
  end

  defp count([], acc), do: acc
  defp count([_ | rest], acc) do
    count(rest, acc + 1)
  end

  @spec reverse(list) :: list
  def reverse(l) do
    reverse(l, [])
  end

  defp reverse([], acc), do: acc
  defp reverse([h | rest], acc) do
    reverse(rest, [h | acc])
  end

  @spec map(list, (any -> any)) :: list
  def map(l, f) do
    map(l, f, [])
  end

  defp map([], _f, acc), do: reverse(acc)
  defp map([h | rest], f, acc) do
    map(rest, f, [f.(h) | acc])
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f) do
    filter(l, f, [])
  end

  defp filter([], _f, acc), do: reverse(acc)
  defp filter([h | rest], f, acc) do
    if f.(h) do
      filter(rest, f, [h | acc])
    else
      filter(rest, f, acc)
    end
  end

  @type acc :: any
  @spec foldl(list, acc, (any, acc -> acc)) :: acc
  def foldl([], acc, _f), do: acc
  def foldl([h | rest], acc, f) do
    foldl(rest, f.(h, acc), f)
  end

  @spec foldr(list, acc, (any, acc -> acc)) :: acc
  def foldr([], acc, _f), do: acc
  def foldr([h | rest], acc, f) do
    f.(h, foldr(rest, acc, f))
  end

  @spec append(list, list) :: list
  def append(a, b), do: do_append(reverse(a), b)

  defp do_append(l, []), do: reverse(l)
  defp do_append(l, [h | rest]), do: do_append([h | l], rest)

  @spec concat([[any]]) :: [any]
  def concat(ll) do
    concat([], ll)
  end

  defp concat(acc, []), do: append(acc, [])
  defp concat(acc, [h | rest]), do: concat(append(acc, h), rest)
end
