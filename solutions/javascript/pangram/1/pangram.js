const valid = 'abcdefghijklmnopqrstuvwxyz'

export const isPangram = (input) => {
  if (input === '') return false
  return (input
    .toLocaleLowerCase()
    .replace(/[^a-z]/g, '')
    .split('')
    .filter(unique)
    .sort()
    .join('') === valid)
};

function unique(value, index, self) {
  return self.indexOf(value) === index;
}