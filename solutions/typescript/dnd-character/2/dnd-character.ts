type Abilities = "strength" | "dexterity" | "constitution" | "intelligence" | "wisdom" | "charisma"

export class DnDCharacter {
  hitpoints = 0;
  strength = 0;
  dexterity = 0;
  constitution = 0;
  intelligence = 0;
  wisdom = 0;
  charisma = 0;

  public static generateAbilityScore() : number {
    return Array(4)
      .fill(0)
      .map(() => Math.round(Math.random() * 6) + 1)
      .sort()
      .slice(0, 3)
      .reduce((acc, curr) => acc + curr)
  }

  public static getModifierFor(abilityValue:number):number {
    return Math.floor((abilityValue - 10) / 2)
  }

  constructor() {
    Object.keys(this)
      .filter(f => f !== 'hitpoints ')
      .forEach(f => this[f as Abilities] = DnDCharacter.generateAbilityScore())
    this.hitpoints = 10 + DnDCharacter.getModifierFor(this.constitution)
  }
}
