export function compute(left: string, right: string): number {
  if ( left.length !== right.length ) {
    throw('DNA strands must be of equal length.');
  }

  return left
    .split('')
    .map((el: string, i: number): number => ( el === right[i] ? 0 : 1))
    .reduce((acc: number, curr: number): number => acc + curr);
}
