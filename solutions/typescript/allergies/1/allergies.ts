const allergens: {[key: string]: number} = {
  "eggs": 1,
  "peanuts": 2,
  "shellfish": 4,
  "strawberries": 8,
  "tomatoes": 16,
  "chocolate": 32,
  "pollen": 64,
  "cats": 128,
};

export class Allergies {
  private allergies: string[];

  constructor(allergenIndex: number) {
    this.allergies = Object.keys(allergens).filter((a) => (
      (allergenIndex & allergens[a]) === allergens[a]
    ));
  }

  public list(): string[] {
    return this.allergies;
  }

  public allergicTo(allergen: string): boolean {
    return this.allergies.includes(allergen);
  }
}