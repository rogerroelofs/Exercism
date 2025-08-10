//
// This is only a SKELETON file for the 'Raindrops' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const convert = (n) => {
  let words = [];
  if ( n % 3 === 0 ) words.push('Pling');
  if ( n % 5 === 0 ) words.push('Plang');
  if ( n % 7 === 0 ) words.push('Plong');
  if ( words.length === 0 ) words.push(n.toString());
  return words.join('');
};
