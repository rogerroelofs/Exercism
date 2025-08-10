export const square = (sq: number): bigint => {
  if ( sq <= 0 || sq > 64 ) throw('whatever');
  return 2n ** BigInt(sq - 1);
}

export const total = (): BigInt => {
  let tot = 0n;
  for ( let i = 1; i <= 64; i++ ) {
    tot += square(i);
  }
  return tot;
}
