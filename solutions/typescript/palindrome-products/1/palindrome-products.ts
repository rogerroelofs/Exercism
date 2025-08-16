interface Input {
  maxFactor: number
  minFactor?: number
}

interface Palindrome { 
  value: number | null; 
  factors: [number, number][];
}

interface Output {
  smallest: Palindrome;
  largest: Palindrome;
}

export function generate(params: Input): Output {
  const { minFactor = 1, maxFactor } = params;
  if (minFactor > maxFactor) {
    throw new Error('min must be <= max');
  }
  const palindromes: Palindrome[] = [];

  for (let i = minFactor; i <= maxFactor; i++) {
    for (let j = i; j <= maxFactor; j++) {
      const product = i * j;
      if (isPalindrome(product)) {
        let palindrome = palindromes.find(p => p.value === product);
        if (!palindrome) {
          palindrome = { value: product, factors: [] };
          palindromes.push(palindrome);
        }
        palindrome.factors.push([i, j]);
      }
    }
  }

  if (palindromes.length === 0) {
    return {
      smallest: {value: null, factors: []},
      largest: {value: null, factors: []}
    };
  }

  const validPalindromes = palindromes.filter(p => p.value !== null);
  const smallest = validPalindromes.reduce((min, p) => (p.value! < min.value!) ? p : min);
  const largest = validPalindromes.reduce((max, p) => (p.value! > max.value!) ? p : max);

  return {
    smallest,
    largest
  };
}

function isPalindrome(num: number): boolean {
  const str = num.toString();
  return str === str.split('').reverse().join('');
}
