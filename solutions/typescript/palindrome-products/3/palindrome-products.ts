interface Input {
  maxFactor: number;
  minFactor?: number;
}
interface SearchResult {
  value: number | null;
  factors: [number, number][];
}
interface Result {
  smallest: SearchResult;
  largest: SearchResult;
}
export function generate(params: Input): Result {
  if ((params.minFactor ?? 1) > params.maxFactor) {
    throw new Error("min must be <= max");
  }
  const result: Result = {
    smallest: { value: null, factors: [] },
    largest: { value: null, factors: [] },
  };
  for (let i = params.minFactor ?? 1; i <= params.maxFactor; i++) {
    for (let j = i; j <= params.maxFactor; j++) {
      const product = i * j;
      const isPalindrome = isPalindromeNumber(product);
      if (!isPalindrome) continue;
      if (result.smallest.value === null || product < result.smallest.value) {
        result.smallest = { value: product, factors: [[i, j]] };
      } else if (result.smallest.value === product) {
        result.smallest.factors.push([i, j]);
      }
      if (result.largest.value === null || product > result.largest.value) {
        result.largest = { value: product, factors: [[i, j]] };
      } else if (result.largest.value === product) {
        result.largest.factors.push([i, j]);
      }
    }
  }
  return result;
}
function isPalindromeNumber(input: number): boolean {
  const numberString = input.toString();
  for (let i = 0; i < numberString.length / 2; i++) {
    if (numberString[i] !== numberString[numberString.length - i - 1]) {
      return false;
    }
  }
  return true;
}