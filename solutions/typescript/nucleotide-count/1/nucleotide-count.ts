type Counts = {
  [key: string]: number
}

export function nucleotideCounts(n: string): Counts {
  let start: Counts = {A: 0, C: 0, G: 0, T: 0};

  return n
    .split('')
    .reduce((acc: Counts, curr: string): Counts => {
      if ( ! ['A', 'C', 'G', 'T'].includes(curr)) {
        throw('Invalid nucleotide in strand');
      }

      acc[curr]++;
      return acc;
    }, start);
}
