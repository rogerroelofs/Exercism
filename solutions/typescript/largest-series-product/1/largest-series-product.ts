export const largestProduct = (input: string, count: number): number => {
  if ( count < 0 ) throw('Span must be greater than zero');
  if ( count > input.length ) throw('Span must be smaller than string length');
  if ( input === '' || count === 0 ) return 1;

  const numbers = input.split('')
    .map((n: string): number => parseInt(n, 10))
    .filter((n: number): boolean => {
      if ( Number.isNaN(n) ) throw('Digits input must only contain digits');
      return true;
    });

  let max = 0;
  for ( let i = 0; i <= numbers.length - count; i++) {
    let sum = numbers
      .slice(i, i + count)
      .reduce((acc: number, cur: number): number => acc * cur);
    if ( sum > max ) max = sum;
  }
  return max;
}
