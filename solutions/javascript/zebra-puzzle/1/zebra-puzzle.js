const colors = ['red', 'green', 'ivory', 'yellow', 'blue'];
const drinks = ['coffee', 'tea', 'milk', 'orange juice', 'water'];
const pets = ['dog', 'snails', 'fox', 'horse', 'zebra'];
const nationalities = ['englishman', 'spaniard', 'ukranian', 'norwegian', 'japanese'];
const smokes = ['old gold', 'kools', 'chesterfields', 'lucky strike', 'parliaments'];

const permutator = (inputArr) => {
  let result = [];

  const permute = (arr, m = []) => {
    if (arr.length === 0) {
      result.push(m)
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permute(curr.slice(), m.concat(next))
     }
   }
 }

 permute(inputArr)

 return result;
}

const PERMUTATIONS = permutator([1,2,3,4,5]);
const rightOf = (a, b) => a === b+1;
const nextTo = (a, b) => rightOf(a, b) || rightOf(b, a);

export class ZebraPuzzle {
  constructor() {
    this.drinkerOfWater = null;
    this.ownerOfZebra = null;
  }

  waterDrinker() {
    if (!this.drinkerOfWater) this.solve();
    return this.drinkerOfWater;
  }

  zebraOwner() {
    if (!this.ownerOfZebra) this.solve();
    return this.ownerOfZebra;
  }


  solve() {
    for (const p of PERMUTATIONS) {
      if (this.solveForColour(p)) break;
    }
  }

  solveForColour(permutation) {
    colors.forEach((color, i) => this[color] = permutation[i]);
    if (
      rightOf(this.green, this.ivory)             // clue 6
    ) {
      for (const p of PERMUTATIONS) {
        if (this.solveForNationality(p))
          return true;
      }
    }
  }

  solveForNationality(permutation) {
    nationalities.forEach((nation, i) => this[nation] = permutation[i]);
    if (
        this.englishman === this.red                 // clue 2
        && this.norwegian === 1               // clue 10
        && nextTo(this.norwegian, this.blue)      // clue 15
    ) {
      this.nationalities = []
      this.nationalities[this.englishman] = "EnglishMan";
      this.nationalities[this.spaniard] = "Spaniard";
      this.nationalities[this.ukranian] = "Ukranian";
      this.nationalities[this.norwegian] = "Norwegian";
      this.nationalities[this.japanese] = "Japanese";
      for (const p of PERMUTATIONS) {
        if (this.solveForBeverages(p))
          return true;
      }
    }
  }

  solveForBeverages(permutation) {
    drinks.forEach((d, i) => this[d] = permutation[i]);
    if (
        this.coffee === this.green                // clue 4
        && this.ukranian === this.tea             // clue 5
        && this.milk === 3                        // clue 9
    ) {
      for (const p of PERMUTATIONS) {
        if (this.solveForSmokes(p))
          return true;
      }
    }
  }


  solveForSmokes(permutation) {
    smokes.forEach((smoke, i) => this[smoke] = permutation[i]);
    if (
        this.kools === this.yellow                // clue 8
        && this['lucky strike'] === this['orange juice']  // clue 13
        && this.japanese === this.parliaments     // clue 14
    ) {
      for (const p of PERMUTATIONS) {
        if (this.solveForPets(p))
          return true;
      }
    }
  }

  solveForPets(permutation) {
    pets.forEach((pet, i) => this[pet] = permutation[i]);
    if (
        this.spaniard === this.dog                 // clue 3
        && this['old gold'] === this.snails           // clue 7
        && nextTo(this.chesterfields, this.fox)   // clue 11
        && nextTo(this.kools, this.horse)         // clue 12
    ) {
      this.drinkerOfWater = this.nationalities[this.water];
      this.ownerOfZebra = this.nationalities[this.zebra];
      return true;
    }
    return false;
  }
}
