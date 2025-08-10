defmodule BasketballWebsite do

  @spec extract_from_path(Access.t, binary) :: any
  def extract_from_path(data, path) do
    get_by_path(data, String.split(path, ["."]))
  end

  @spec get_in_path(Access.t, binary) :: any
  def get_in_path(data, path) do
    get_in(data, String.split(path, ["."]))
  end

  @spec get_by_path(Access.t, List.t) :: any
  defp get_by_path(data, []), do: data
  defp get_by_path(data, [head | tail]) do
    get_by_path(data[head], tail)
  end

end
