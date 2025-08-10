export function transpose(strings: string[]): string[] {
  if (strings.length === 0) return [];

  const maxLength = Math.max(...strings.map((str) => str.length));
  const result: string[] = Array(maxLength).fill('');
  // turn columns into rows
  for (let col = 0; col < maxLength; col++) {
    for (let row = 0; row < strings.length; row++) {
      result[col] += (strings[row][col] || ' ');
    }
  }
  result[result.length - 1] = result[result.length - 1].trimEnd();

  return result;
}
