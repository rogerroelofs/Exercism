type Direction = 'encode' | 'decode'

function make_key(length: number): string {
  let result = '';
  const characters = 'abcdefghijklmnopqrstuvwxyz';
  const len = characters.length;
  for ( var i = 0; i < length; i++ ) {
    result += characters.charAt(Math.floor(Math.random() * len));
  }
  return result;
}

const a_offset = 'a'.charCodeAt(0)

export class SimpleCipher {
  key = '';
  offsets: number[]
  keyLength = 0;

  constructor(key?: string) {
    this.key = (key) ? key : make_key(100);
    this.offsets = [...this.key].map((char) => char.charCodeAt(0) - a_offset)
    this.keyLength = this.key.length;
  }

  substitute(input: string, direction: Direction): string {
    const dir = (direction == 'encode') ? 1 : -1;
    return input.replace(/[a-z]/g, (char, i) => {
      const diff = char.charCodeAt(0) - a_offset
      let offset = diff + (dir * this.offsets[i % this.keyLength])
      if (offset < 0) offset += 26
      return String.fromCharCode((offset % 26) + a_offset)
    })
  }

  encode(plain: string): string {
    return this.substitute(plain, 'encode');
  }

  decode(cypher: string): string {
    return this.substitute(cypher, 'decode');
  }
}
