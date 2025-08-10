const PLAIN = 'abcdefghijklmnopqrstuvwxyz'.split('');
const CYPHER = [...PLAIN].reverse();

function do_cypher(input: string, find: string[], replace: string[]): string {
  return input.split('').map((char: string) => {
    const i = find.indexOf(char);
    return (i >= 0) ? replace[i] : char;
  }).join('')
}

export function encode(plainText: string): string {
  let str: string = plainText
    .replace(/[^a-z0-9]/gi, '')
    .toLowerCase();

  const parts = do_cypher(str, PLAIN, CYPHER)
    .match(/.{1,5}/g); // split into 5 char chunks
  return (parts) ? parts.join(' ') : '';
}

export function decode(cipherText: string): string {
  const str = cipherText
    .replace(/[^a-z0-9]/gi, '')
    .toLowerCase();
  return do_cypher(str, CYPHER, PLAIN);
}
