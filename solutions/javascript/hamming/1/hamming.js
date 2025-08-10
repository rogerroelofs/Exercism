export const compute = (left, right) => {
  if ( left.length !== right.length ) {
    throw new Error('strands must be of equal length');
  }

  return left
    .split('')
    .map((el, i) => ( el === right[i] ? 0 : 1))
    .reduce((acc, curr) => acc + curr, 0);
}