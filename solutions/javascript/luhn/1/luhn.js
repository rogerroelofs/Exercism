//
// This is only a SKELETON file for the 'Luhn' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const valid = (digitString) => {
  if ( digitString.match(/[-A-Za-z$]/g) ) return false;
  digitString = digitString.replace(/[^0-9]/g, '')

  if ( digitString.length <= 1) return false;

  const total = digitString
    .split('')
    .reverse()
    .map((digit) => {
      const parsed = parseInt(digit, 10);
      return parsed;
    })
    .map((digit, i) => {
      if ( i % 2 !== 0 ) {
        digit *= 2;
        digit = ( digit > 9 ) ? digit - 9 : digit;
      }
      return digit;
    })
    .reduce((acc, cur) => acc + cur, 0);

  return total % 10 === 0;
};
