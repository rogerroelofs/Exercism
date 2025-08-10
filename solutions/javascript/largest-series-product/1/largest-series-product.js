export const largestProduct = (input, count) => {
  if ( count < 0 ) throw Error('Span must be greater than zero');
  if ( count > input.length ) throw Error('Span must be smaller than string length');
  if ( input === '' || count === 0 ) return 1;

  const numbers = input.split('')
    .map((n) => parseInt(n, 10))
    .filter((n) => {
      if ( Number.isNaN(n) ) throw Error('Digits input must only contain digits');
      return true;
    });

  let max = 0;
  for ( let i = 0; i <= numbers.length - count; i++) {
    let sum = numbers
      .slice(i, i + count)
      .reduce((acc, cur) => acc * cur);
    if ( sum > max ) max = sum;
  }
  return max;
};
