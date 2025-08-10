export const parse = (phrase) => {
  return (phrase
    .toUpperCase()
    .match(/[A-Z']+/g)
    .map(s => s[0])
    .join(''));
};
