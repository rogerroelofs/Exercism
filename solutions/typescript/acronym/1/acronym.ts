
export function parse(phrase: string): string {
  const caps: RegExpMatchArray | null =
    phrase.match(/(\b\w|(?<=[a-z])[A-Z])/g);
  if ( caps === null ) {
    return '';
  } else {
    return caps
    .map((s: string): string => s.toUpperCase())
    .join('');
  }
}
