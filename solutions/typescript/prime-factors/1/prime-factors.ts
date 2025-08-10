export function calculatePrimeFactors(number: number): number[] {
  const factors: number[] = [];
  let divisor = 2;

  while (number > 1) {
    if (number % divisor === 0) {
      factors.push(divisor);
      number /= divisor;
    } else {
      divisor++;
    }
  }

  return factors;
}
