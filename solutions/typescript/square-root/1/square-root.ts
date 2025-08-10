export function squareRoot(radicand: number): number {
  if (radicand < 0) throw new Error('Negative input not allowed');
  if ( radicand <= 1 ) return radicand;

  let max = radicand / 2;
  let test = (max + radicand / max) / 2;
  while (test < max) {
    max = test;
    test = (max + radicand / max) / 2;
  }
  return max;
}
