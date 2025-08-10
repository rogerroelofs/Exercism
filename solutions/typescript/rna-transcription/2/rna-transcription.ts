type Nucleotide = 'G' | 'C' | 'A' | 'T'

const TRANSCRIBE: { [key in Nucleotide]: string; } = {
  G: 'C',
  C: 'G',
  T: 'A',
  A: 'U'
}

function decodeChar(char:string) : string {
  if ( ! (char in TRANSCRIBE) ) {
    throw("Invalid input DNA.")
  }
  return TRANSCRIBE[char as Nucleotide]
}

export function toRna(dna:string) : string {
  return dna.split('')
  .map(char => decodeChar(char))
  .join('')
}
