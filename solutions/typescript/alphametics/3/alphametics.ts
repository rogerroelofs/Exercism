type ltd  = { [key: string]: number };

export function solve(puzzle: string): Record<string, number> | undefined {
  // Split the puzzle into left-side words and right-side result
  const [leftSide, rightSide] = puzzle.split(' == ');
  const words = leftSide.split(' + ');
  const result = rightSide;
  // console.log(words, result);

  // Get unique letters in the puzzle
  const letters = new Set([...words.join(''), ...result]);

  // Create a dictionary to store letter-to-digit mappings
  const letterToDigit: ltd = {};

  function backtrack(idx: number): ltd | undefined {
    if (idx === letters.size) {
      // All letters have been assigned digits
      const leftSideSum = words.reduce((sum, word) => sum + wordToNum(word, letterToDigit), 0);
      const rightSideNum = wordToNum(result, letterToDigit);
      if (leftSideSum === rightSideNum) {
        return letterToDigit;
      }
      return undefined;
    }

    const letter = Array.from(letters)[idx];
    for (let digit = 0; digit <= 9; digit++) {
      if (digit === 0 && [...words, result].some(word => word[0] === letter)) {
        continue; // Leading digit cannot be zero
      }
      if (Object.values(letterToDigit).includes(digit)) {
        continue; // Digit already assigned to another letter
      }
      letterToDigit[letter] = digit;
      const solution = backtrack(idx + 1);
      if (solution !== undefined) {
        return solution;
      }
      delete letterToDigit[letter];
    }

    return undefined;
  }

  return backtrack(0);
}

function wordToNum(word: string, mapping: { [key: string]: number }): number {
  return parseInt(word.split('').map(letter => mapping[letter]).join(''), 10);
}
