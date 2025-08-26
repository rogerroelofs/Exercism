interface Input {
  maxFactor: number
  minFactor?: number
}

interface Palindrome { 
  value: number; 
  factors: [number, number][];
}

interface Output {
  smallest: Palindrome | {value: null, factors: []};
  largest: Palindrome | {value: null, factors: []};
}

export function generate(params: Input): Output {
  const { minFactor = 1, maxFactor } = params;
  if (minFactor > maxFactor) {
    throw new Error('min must be <= max');
  }
  const palindromeMap = new Map<number, Palindrome>();

  for (let i = minFactor; i <= maxFactor; i++) {
    for (let j = i; j <= maxFactor; j++) {
      const product = i * j;
      if (isPalindrome(product)) {
        let palindrome = palindromeMap.get(product);
        if (!palindrome) {
          palindrome = { value: product, factors: [] };
          palindromeMap.set(product, palindrome);
        }
        palindrome.factors.push([i, j]);
      }
    }
  }

  if (palindromeMap.size === 0) {
    return {
      smallest: {value: null, factors: []},
      largest: {value: null, factors: []}
    };
  }

  const palindromes = Array.from(palindromeMap.values());
  palindromes.sort((a, b) => a.value - b.value);
  
  return {
    smallest: palindromes[0],
    largest: palindromes[palindromes.length - 1]
  };
}

function isPalindrome(num: number): boolean {
  const str = num.toString();
  return str === str.split('').reverse().join('');
}
