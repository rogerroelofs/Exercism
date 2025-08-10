export class Anagram {
  #letters: string;
  constructor(input: string) {
    this.#letters = input.toLowerCase();
  }

  public matches(...potentials: string[]): string[] {
    const cmp: string = this.#letters.split('').sort().join('');
    return potentials.filter((str): boolean => {
      str = str.toLowerCase();
      if ( str === this.#letters ) return false;
      const test = str.split('').sort().join('');
      return test === cmp;
    });
  }
}
