export function convert(n: number): string {
  let words: string[] = [];
  if ( n % 3 === 0 ) words.push('Pling');
  if ( n % 5 === 0 ) words.push('Plang');
  if ( n % 7 === 0 ) words.push('Plong');
  if ( words.length === 0 ) words.push(n.toString());
  return words.join('');
}
