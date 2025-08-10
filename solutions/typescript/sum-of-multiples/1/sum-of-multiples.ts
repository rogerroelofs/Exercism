export function sum(multiples: number[], max: number): number {
  let total = 0;
  for ( let i = 1; i < max; i++ ) {
    const matches = multiples.filter((c: number): boolean => {
      return i % c === 0;
    })
    if ( matches.length > 0 ) total += i;
  }
  return total;
}
