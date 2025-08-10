defmodule TopSecret do
  @spec to_ast(String.t) :: tuple
  def to_ast(string) do
    Code.string_to_quoted!(string)
  end

  @spec decode_secret_message_part(tuple, list) :: {tuple, list}
  def decode_secret_message_part({op, _, args} = ast, acc) when op in [:def, :defp] do
    [func_ast | _] = args
    {ast, [calc_func_name(func_ast) | acc]}
  end
  def decode_secret_message_part(ast, acc) do
    {ast, acc}
  end

  def decode_secret_message(string) do
    ast = to_ast(string)
    {_, acc} = Macro.prewalk(ast, [], &decode_secret_message_part/2)
    acc |> Enum.reverse() |> Enum.join()
  end

  @spec calc_func_name(tuple) :: String.t
  defp calc_func_name(func_ast) do
    func_ast = case elem(func_ast, 0) do
      :when -> elem(func_ast, 2) |> hd
      _ -> func_ast
    end
    name = Atom.to_string(elem(func_ast, 0))
    args = elem(func_ast, 2)
    arity = case args do
      nil -> 0
      _ -> length(args)
    end
    String.slice(name, 0, arity)
  end
end
