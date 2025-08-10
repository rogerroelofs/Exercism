const reconstruct = (word: string, match: RegExpMatchArray): string => (
  word.substring(0 + match[1].length) + match[1] + "ay"
)

const xlateWord = (input: string): string => {
  const word = input.toLowerCase();

  const vow = /^([aeiou]|xr|yt)+/;
  const cons = /^([^aeiouy]+)/;
  const consQU = /^([^aeiouy]*qu)/;
  const wordY = /^y/;
  let match: RegExpMatchArray | null;

  if ( word.match(vow) ) return word + 'ay';
  if ( match = word.match(consQU) ) return reconstruct(word, match);
  if ( match = word.match(cons) ) return reconstruct(word, match);
  if ( match = word.match(wordY) ) return word.substring(1) + 'yay';

  return '';
}

export function translate(input: string):string {
  return input.split(" ").map(word => xlateWord(word)).join(' ');
}
