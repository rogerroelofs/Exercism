function range(start: number, end: number): number[] {
  return Array(end - start + 1).fill(0).map((_, idx) => start + idx)
}

const multipliers: number[] = range(1, 10).reverse();

function validate(isbn: string): boolean {
  // digits and dash for all but the check digit
  let matches: null | RegExpMatchArray = isbn.substring(0, isbn.length - 1).match(/[^-0-9]/);
  if ( matches !== null ) return false;

  // digits and X for the check digit
  matches = isbn.substring(isbn.length).match(/[^0-9X]/);
  if ( matches !== null ) return false;

  isbn = isbn.replace(/-/g, '');
  if ( isbn === '' ) return false;

  return true;
}

export function isValid(isbn: string): boolean {
  if ( validate(isbn) === false ) return false;

  const total: number = isbn.replace(/-/g, '').split('')
    .map((c: string, i: number): number => (
      (c === 'X') ? 10 : parseInt(c, 10)) * multipliers[i]
    )
    .reduce((acc: number, cur: number): number => acc + cur);

  return (total % 11 === 0)
}
