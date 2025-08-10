//
// This is only a SKELETON file for the 'Allergies' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export class Allergies {
  // order of allergens matches the order of the bits in the input number
  static ALLERGENS = [
    'eggs',
    'peanuts',
    'shellfish',
    'strawberries',
    'tomatoes',
    'chocolate',
    'pollen',
    'cats',
  ];
  constructor(allergies) {
    this.allergies = allergies;
  }

  list() {
    // collect all the allergens that are present
    return Allergies.ALLERGENS.filter((allergen) => this.allergicTo(allergen));
  }

  allergicTo(allergen) {
    // calc the bit position of the allergen
    const allergenIndex = Allergies.ALLERGENS.indexOf(allergen);
    // compare the bit position with the bit of the allergen
    return (this.allergies & (1 << allergenIndex)) > 0;
  }
}
