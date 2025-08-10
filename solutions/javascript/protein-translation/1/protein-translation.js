//
// This is only a SKELETON file for the 'Protein Translation' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

const TX = {
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

export const translate = (input) => {
  const codons = (input ?? '').match(/.{1,3}/g);
  if ( Array.isArray(codons) ) {
    const index = codons.findIndex((el) => ['UAA', 'UAG', 'UGA'].includes(el));
    if ( index >= 0 ) {
      codons.splice(index, 999);
    }

    const validCodons = codons.filter((item) => (item in TX));
    if ( codons.length != validCodons.length ) throw new Error("Invalid codon");
    return validCodons.map((item) => TX[item])
  }
  return [];
};
