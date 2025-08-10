defmodule RPG do
  defmodule Character do
    defstruct health: 100, mana: 0
  end

  defmodule LoafOfBread do
    defstruct [health: 5]
  end

  defmodule ManaPotion do
    defstruct strength: 10
  end

  defmodule Poison do
    defstruct []
  end

  defmodule EmptyBottle do
    defstruct []
  end

  defprotocol Edible do
    def eat(item, character)
  end

  defimpl Edible, for: LoafOfBread do
    @spec eat(%RPG.LoafOfBread{}, %RPG.Character{}) :: {nil, %RPG.Character{}}
    def eat(item, character) do
      newChar = struct(character, health: character.health + item.health)
      {nil, newChar}
    end
  end

  defimpl Edible, for: ManaPotion do
    @spec eat(%RPG.ManaPotion{}, %RPG.Character{}) :: {%RPG.EmptyBottle{}, %RPG.Character{}}
    def eat(item, character) do
      newChar = struct(character, mana: character.mana + item.strength)
      {%RPG.EmptyBottle{}, newChar}
    end
  end

  defimpl Edible, for: Poison do
    @spec eat(%RPG.Poison{}, %RPG.Character{}) :: {%RPG.EmptyBottle{}, %RPG.Character{}}
    def eat(_, character) do
      newChar = struct(character, health: 0)
      {%RPG.EmptyBottle{}, newChar}
    end
  end

end
