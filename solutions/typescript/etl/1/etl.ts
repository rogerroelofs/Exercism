interface In {
  [score: string]: string[]
}
interface Out {
  [letter: string]: number
}

export function transform(input: In): Out {
  return Object.entries(input).reduce((acc: Out, [score, letters]) => {
    letters.forEach(letter => acc[letter.toLowerCase()] = parseInt(score, 10));

    return acc;
  }, {} as Out);
}
