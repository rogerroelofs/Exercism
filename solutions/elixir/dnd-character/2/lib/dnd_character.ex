defmodule DndCharacter do
  @type t :: %__MODULE__{
          strength: pos_integer(),
          dexterity: pos_integer(),
          constitution: pos_integer(),
          intelligence: pos_integer(),
          wisdom: pos_integer(),
          charisma: pos_integer(),
          hitpoints: pos_integer()
        }

  defstruct ~w[strength dexterity constitution intelligence wisdom charisma hitpoints]a

  @spec modifier(pos_integer()) :: integer()
  def modifier(score) do
    floor((score - 10) / 2)
  end

  @spec ability :: pos_integer()
  def ability do
    1..4
    |> Enum.map(fn _ -> :rand.uniform(6) end)
    |> Enum.sort()
    |> List.delete_at(0)
    |> Enum.sum()
  end

  @spec character :: t()
  def character do
    struct(__MODULE__)
    |> Map.keys()
    |> Enum.reject(&(&1 == :__struct__))
    |> Enum.reduce(%__MODULE__{}, &get_ability/2)
  end

  @spec get_ability(atom, t()) :: t()
  def get_ability(:hitpoints, acc) do
    Map.put(acc, :hitpoints, 10 + modifier(acc.constitution))
  end

  def get_ability(k, acc) do
    Map.put(acc, k, ability())
  end
end
