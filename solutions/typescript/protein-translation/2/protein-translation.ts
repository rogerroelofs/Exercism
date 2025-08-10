type TXType = {
  [key: string]: string
}
const TX: TXType = {
  AUG: 'Methionine',
  UUU: 'Phenylalanine',
  UUC: 'Phenylalanine',
  UUA: 'Leucine',
  UUG: 'Leucine',
  UCU: 'Serine',
  UCC: 'Serine',
  UCA: 'Serine',
  UCG: 'Serine',
  UAU: 'Tyrosine',
  UAC: 'Tyrosine',
  UGU: 'Cysteine',
  UGC: 'Cysteine',
  UGG: 'Tryptophan',
  // UAA, UAG, UGA	STOP
}

export function translate(input: string): string[] {
  const codons = input.match(/.{1,3}/g);
  if ( Array.isArray(codons) ) {
    const index:number = codons.findIndex((el) => ['UAA', 'UAG', 'UGA'].includes(el));
    if ( index >= 0 ) {
      codons.splice(index, 999);
    }

    const validCodons = codons.filter((item:string):boolean => (item in TX));
    if ( codons.length != validCodons.length ) throw new Error("Invalid codon");
    return validCodons.map((item: string): string => TX[item])
  }
  return [];
}
