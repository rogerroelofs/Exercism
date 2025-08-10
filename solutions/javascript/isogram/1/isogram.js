export const isIsogram = (input) => {
  if ( input === '' ) return true;

  const counts = input
    .toLowerCase()
    .replace(/[ -]/g, '')
    .split('')
    .reduce((acc, char) => {
      if ( acc[char] === undefined ) {
        acc[char] = 1;
      } else {
        acc[char]++;
      }
      return acc;
    }, {});

  return Object.values(counts).sort().reverse()[0] === 1;
}