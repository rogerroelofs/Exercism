type RomanMap = { [key: string]: number };

// for this algorithm map order must be from high to low
const map: RomanMap = {
  M: 1000,
  CM: 900,
  D: 500,
  CD: 400,
  C: 100,
  XC: 90,
  L: 50,
  XL: 40,
  X: 10,
  IX: 9,
  V: 5,
  IV: 4,
  I: 1,
};

export const toRoman = (n: number): string => {
  let roman = '';
  for (const i in map) {
      while (n >= map[i]) {
          roman += i;
          n -= map[i];
      }
  }
  return roman;
}
