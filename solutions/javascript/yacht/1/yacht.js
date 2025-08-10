const rules = {
  ones: (dice) => dice.filter((d) => d === 1).length,
  twos: (dice) => dice.filter((d) => d === 2).length * 2,
  threes: (dice) => dice.filter((d) => d === 3).length * 3,
  fours: (dice) => dice.filter((d) => d === 4).length * 4,
  fives: (dice) => dice.filter((d) => d === 5).length * 5,
  sixes: (dice) => dice.filter((d) => d === 6).length * 6,
  'full house': (dice) => {
    const counts = dice.reduce((acc, d) => {
      acc[d] = acc[d] + 1 || 1;
      return acc;
    }, {});
    return (Object.values(counts).sort().join('') === '23') ? dice.reduce((acc, d) => acc + d, 0) : 0;
  },
  'four of a kind': (dice) => {
    const counts = dice.reduce((acc, d) => {
      acc[d] = acc[d] + 1 || 1;
      return acc;
    }, {});
    return Object.entries(counts).reduce((acc, [k, v]) => (v >= 4) ? k * 4 : acc, 0);
  },
  'little straight': (dice) => (dice.sort().join('') === '12345') ? 30 : 0,
  'big straight': (dice) => (dice.sort().join('') === '23456') ? 30 : 0,
  choice: (dice) => dice.reduce((acc, d) => acc + d, 0),
  yacht: (dice) => (dice.every((d) => d === dice[0])) ? 50 : 0,
};

export const score = (dice, category) => {
  if (!rules[category]) {
    throw new Error('Invalid category');
  }
  return rules[category](dice);
};
