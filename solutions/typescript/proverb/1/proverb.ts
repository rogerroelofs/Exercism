const chunk = (array: string[], chunkSize: number) => {
  var retArr: string[][] = [];
  for (let i = 0; i < array.length - (chunkSize - 1); i++) {
      retArr.push(array.slice(i, i + chunkSize));
  }
  return retArr;
}

export function proverb(...input: string[]): string {
  const chunks = chunk(input, 2);
  const ret: string[] = chunks.map(([a, b]: string[]): string => (
    `For want of a ${a} the ${b} was lost.`
  ));

  return ret.join("\n") + "\n" + `And all for the want of a ${input[0]}.`;
}
