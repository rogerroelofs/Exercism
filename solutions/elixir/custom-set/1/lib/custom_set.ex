defmodule CustomSet do
  @opaque t :: %__MODULE__{map: map}

  defstruct map: %{}

  @spec new(Enum.t()) :: t
  def new(enumerable) do
    enumerable |> Enum.reduce(%__MODULE__{}, fn item, acc -> add(acc, item) end)
  end

  @spec empty?(t) :: boolean
  def empty?(custom_set) do
    Map.equal?(custom_set.map, %{})
  end

  @spec contains?(t, any) :: boolean
  def contains?(custom_set, element) do
    Map.has_key?(custom_set.map, element)
  end

  @spec subset?(t, t) :: boolean
  def subset?(custom_set_1, custom_set_2) do
    custom_set_1.map |> Map.keys() |> Enum.all?(&contains?(custom_set_2, &1))
  end

  @spec disjoint?(t, t) :: boolean
  def disjoint?(custom_set_1, custom_set_2) do
    custom_set_1.map |> Map.keys() |> Enum.any?(&contains?(custom_set_2, &1)) |> Kernel.not
  end

  @spec equal?(t, t) :: boolean
  def equal?(custom_set_1, custom_set_2) do
    Map.equal?(custom_set_1.map, custom_set_2.map)
  end

  @spec add(t, any) :: t
  def add(custom_set, element) do
    %__MODULE__{custom_set | map: custom_set.map |> Map.put(element, nil)}
  end

  @spec intersection(t, t) :: t
  def intersection(custom_set_1, custom_set_2) do
    %__MODULE__{map: custom_set_2.map |> Map.take(custom_set_1.map |> Map.keys())}
  end

  @spec difference(t, t) :: t
  def difference(custom_set_1, custom_set_2) do
    %__MODULE__{map: custom_set_1.map |> Map.drop(custom_set_2.map |> Map.keys())}
  end

  @spec union(t, t) :: t
  def union(custom_set_1, custom_set_2) do
    %__MODULE__{map: Map.merge(custom_set_1.map, custom_set_2.map)}
  end
end
