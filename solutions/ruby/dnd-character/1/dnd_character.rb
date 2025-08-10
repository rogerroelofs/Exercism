=begin
Write your code for the 'D&D Character' exercise in this file. Make the tests in
`dnd_character_test.rb` pass.

To get started with TDD, see the `README.md` file in your
`ruby/dnd-character` directory.
=end

class DndCharacter
  attr_reader :strength, :dexterity, :constitution, :intelligence, :wisdom, :charisma, :hitpoints
  def self.modifier(score)
    (score - 10) / 2
  end

  def initialize
    @strength = DndCharacter.ability
    @dexterity = DndCharacter.ability
    @constitution = DndCharacter.ability
    @intelligence = DndCharacter.ability
    @wisdom = DndCharacter.ability
    @charisma = DndCharacter.ability
    @hitpoints = 10 + DndCharacter.modifier(@constitution)
  end

  private

  def self.ability
    rolls = Array.new(4) { rand(1..6) }
    rolls.sort!.shift
    rolls.sum
  end
end
