export function primes(max: number): number[] {
  if (max < 2) return [];
  const sieve = new Array(max + 1).fill(true);
  sieve[0] = sieve[1] = false;
  // Mark even numbers > 2 as non-prime
  for (let i = 4; i <= max; i += 2) {
    sieve[i] = false;
  }
  // Only check odd numbers starting from 3
  for (let i = 3; i * i <= max; i += 2) {
    if (sieve[i]) {
      for (let j = i * i; j <= max; j += 2 * i) {
        sieve[j] = false;
      }
    }
  }
  // Collect primes: 2 and all odd numbers marked true
  const primes: number[] = [2];
  for (let i = 3; i <= max; i += 2) {
    if (sieve[i]) primes.push(i);
  }
  return max >= 2 ? primes : [];
}
