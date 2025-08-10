//
// This is only a SKELETON file for the 'D&D Character' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const abilityModifier = (constitution) => {
  if ( constitution > 18 ) throw new Error('Ability scores can be at most 18');
  if ( constitution < 3 ) throw new Error('Ability scores must be at least 3');
  return Math.floor((constitution / 2) - 5);
};

export class Character {
  #strength = this.getAbility('strength');
  #dexterity = this.getAbility('dexterity');
  #constitution = this.getAbility('constitution');
  #intelligence = this.getAbility('intelligence');
  #wisdom = this.getAbility('wisdom');
  #charisma = this.getAbility('charisma');
  #hitpoints = 10 + abilityModifier(this.#constitution);

  static rollAbility() {
    return Math.floor(Math.random() * 16) + 3;
  }

  getAbility(field) {
    const fName = `#${field}`;
    if (!this[fName]) {
      this[fName] = this.constructor.rollAbility();
    }
    return this[fName];
  }

  get strength() {
    return this.#strength;
  }

  get dexterity() {
    return this.#dexterity;
  }

  get constitution() {
    return this.#constitution;
  }

  get intelligence() {
    return this.#intelligence;
  }

  get wisdom() {
    return this.#wisdom;
  }

  get charisma() {
    return this.#charisma;
  }

  get hitpoints() {
    return this.#hitpoints;
  }
}
