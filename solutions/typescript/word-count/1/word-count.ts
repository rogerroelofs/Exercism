interface Counts {
  [key: string]: number
}

export function count(input: string): Map<string, number> {
  const words = input
    .trim()
    .split(/\s+/)
    .map((word: string) => word.toLowerCase());
  const counts: Counts = words.reduce(calc, {} as Counts);

  return new Map(Object.entries(counts))
}

function calc(acc: Counts, word :string): Counts {
  if ( typeof acc[word] === 'number' ) {
    acc[word] = acc[word] + 1;
  } else {
    acc[word] = 1;
  }
  return acc;
}
