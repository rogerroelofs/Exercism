export class DnDCharacter {
  strength = DnDCharacter.generateAbilityScore();
  dexterity = DnDCharacter.generateAbilityScore();
  constitution = DnDCharacter.generateAbilityScore();
  intelligence = DnDCharacter.generateAbilityScore();
  wisdom = DnDCharacter.generateAbilityScore();
  charisma = DnDCharacter.generateAbilityScore();
  hitpoints = 10 + DnDCharacter.getModifierFor(this.constitution);

  public static generateAbilityScore() : number {
    return Array(4)
      .fill(0)
      .map(() => Math.round(Math.random() * 6))
      .sort()
      .slice(1)
      .reduce((acc, curr) => acc + curr)
  }

  public static getModifierFor(abilityValue:number):number {
    return Math.floor((abilityValue - 10) / 2)
  }
}
