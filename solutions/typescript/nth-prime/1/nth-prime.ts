export function nth(n: number): number {
  if (n < 1) throw new Error('Prime is not possible');

  const primes = [];
  let candidate = 2;

  while (primes.length < n) {
    if (isPrime(candidate)) {
      primes.push(candidate);
    }
    candidate++;
  }

  return primes[n - 1];
}

function isPrime(num: number): boolean {
  for (let i = 2; i * i <= num; i++) {
    if (num % i === 0) return false;
  }
  return num > 1;
}
