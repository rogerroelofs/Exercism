type Counts = {
  [char: string]: number
}

export function isIsogram(input: string): boolean {
  if ( input === '' ) return true;

  const counts = input
    .toLowerCase()
    .replace(/[ -]/g, '')
    .split('')
    .reduce((acc: Counts, char: string): Counts => {
      if ( acc[char] === undefined ) {
        acc[char] = 1;
      } else {
        acc[char]++;
      }
      return acc;
    }, {} as Counts);

  return Object.values(counts).sort().reverse()[0] === 1;
}
