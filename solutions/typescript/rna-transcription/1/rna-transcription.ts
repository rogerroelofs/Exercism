const TRANSCRIBE = {
  G: 'C',
  C: 'G',
  T: 'A',
  A: 'U'
}

function decodeChar(char:string) : string {
  if ( ! (char in TRANSCRIBE) ) {
    throw("Invalid input DNA.")
  }
  // @ts-ignore
  return TRANSCRIBE[char]
}

export function toRna(dna:string) : string {
  return dna.split('')
  .map(char => decodeChar(char))
  .join('')
}
