export function valid(digitString: string): boolean {
  if ( digitString.match(/[-A-Za-z$]/g) ) return false;
  digitString = digitString.replace(/ /g, '')

  if ( digitString.length <= 1) return false;

  const total: number = digitString
    .split('')
    .reverse()
    .map((digit: string): number => {
      const parsed = parseInt(digit, 10);
      if ( Number.isNaN(parsed) ) throw('wat!');
      return parsed;
    })
    .map((digit: number, i: number): number => {
      if ( i % 2 !== 0 ) {
        digit *= 2;
        digit = ( digit > 9 ) ? digit - 9 : digit;
      }
      return digit;
    })
    .reduce((acc: number, cur: number): number => acc + cur, 0);

  return total % 10 === 0;
}
