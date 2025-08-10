// letters      A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P   Q  R  S  T  U  V  W  X  Y  Z
const scores = [1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10];

export const score = (word) => {
  if ( word === undefined ) return 0;

  return word
    .toUpperCase()
    .replace(/[^A-Z]/g, '')
    .split('')
    .reduce((acc, curr) => (
      acc + scores[curr.charCodeAt(0) - 'A'.charCodeAt(0)]
    ), 0);
};
