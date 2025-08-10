export class DnDCharacter {
  readonly strength = DnDCharacter.generateAbilityScore();
  readonly dexterity = DnDCharacter.generateAbilityScore();
  readonly constitution = DnDCharacter.generateAbilityScore();
  readonly intelligence = DnDCharacter.generateAbilityScore();
  readonly wisdom = DnDCharacter.generateAbilityScore();
  readonly charisma = DnDCharacter.generateAbilityScore();
  readonly hitpoints = 10 + DnDCharacter.getModifierFor(this.constitution);

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
