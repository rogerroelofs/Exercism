const SHIFT_AMT = 7
const MSB = 1 << SHIFT_AMT
const MASK = MSB ^ 0xff  // 0b01111111

// using math instead of bit shift due to overflow issues
function shiftLeft(n: number, shift: number): number {
  return n * (2 ** shift);
}
function shiftRight(n: number, shift: number): number {
  return Math.floor(n / (2 ** shift));
}

export function encode(input: number[]): number[] {
  let res: number[] = [];
  input.forEach((n) => {
    const bytes = [];
    let tmp = n;
    let msb = 0;
    do {
      bytes.unshift((tmp & MASK) | msb);
      msb = MSB;
      tmp = shiftRight(tmp, SHIFT_AMT);
    } while (tmp > 0);
    res = res.concat(bytes);
  })
  return res;
}

export function decode(input: number[]): number[] {
  if ((input[input.length - 1] & MSB) !== 0) {
    throw new Error('Incomplete sequence');
  }

  const accum: { nums: number[], n: number } = {nums: [], n: 0};
  const { nums } = input.reduce((acc, b) => {
    acc.n = shiftLeft(acc.n, SHIFT_AMT) + (b & MASK);
    if ((b & MSB) === 0) {
      acc.nums.push(acc.n);
      acc.n = 0;
    }
    return acc;
  }, accum);
  return nums;
}
