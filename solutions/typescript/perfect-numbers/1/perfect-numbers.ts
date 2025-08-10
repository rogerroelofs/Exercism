function aliquot_sum(num: number): number {
  let f: number[] = [];
  for(let i = 1; i <= num; i++) {
    if(num % i === 0) {
      f.push(i);
    }
  }
  f.splice(-1);
  return f.reduce((acc: number, cur: number): number => acc + cur);
}

export function classify(n: number): string {
  if ( n < 1 ) throw('Classification is only possible for natural numbers.');
  if ( n === 1 ) return 'deficient';

  const a = aliquot_sum(n);
  if ( a === n ) return 'perfect';
  return ( a < n ) ? 'deficient' : 'abundant';
}

