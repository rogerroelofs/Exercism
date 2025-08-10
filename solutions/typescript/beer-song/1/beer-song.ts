const bottles = (count: number) => count == 1 ? 'bottle' : 'bottles';
const countWord = (count: number) => count == 0 ? 'no more' : `${count}`;
const action = (count: number) => {
  const takeDown = count == 1 ? 'it' : 'one';
  const start = count == 1 ? 'Take it down' : 'Take one down';
  if (count == 0) {
    return 'Go to the store and buy some more, 99 bottles of beer on the wall.';
  }
  return `${start} and pass it around, ${countWord(count - 1)} ${bottles(count - 1)} of beer on the wall.`;
}
const capitalize = (word: string) => word.charAt(0).toUpperCase() + word.slice(1);
export function verse(index: number): string {
  const first = capitalize(countWord(index));
  return `${first} ${bottles(index)} of beer on the wall, ${countWord(index)} ${bottles(index)} of beer.
${action(index)}
`;
}

export function sing(
  initialBottlesCount?: number,
  takeDownCount: number = 0
): string {
  const start = initialBottlesCount ?? 99;
  const end = takeDownCount ?? 0;
  const verses = [];
  for (let i = start; i >= end; i--) {
    verses.push(verse(i));
  }
  return verses.join('\n');
}
