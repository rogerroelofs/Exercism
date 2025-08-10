function assert_original_valid(phone: string): void {
  if ( phone.match(/[A-Za-z]/) ) throw('Letters not permitted');
  if ( phone.match(/[@:!]/) ) throw('Punctuations not permitted');
}

function sanitize_cleaned(cleaned: string): string {
  if ( cleaned.length === 11 && cleaned[0] !== '1' ) throw('11 digits must start with 1');
  if ( cleaned.length === 11 ) cleaned = cleaned.substring(1);
  if ( cleaned.length > 10 ) throw('More than 11 digits');
  if ( cleaned.length !== 10 ) throw('Incorrect number of digits');
  if ( cleaned[0] === '0' ) throw('Area code cannot start with zero');
  if ( cleaned[0] === '1' ) throw('Area code cannot start with one');
  if ( cleaned[3] === '0' ) throw('Exchange code cannot start with zero');
  if ( cleaned[3] === '1' ) throw('Exchange code cannot start with one');
  return cleaned;
}
export function clean(phone: string): string {
  assert_original_valid(phone);
  return sanitize_cleaned(phone.replace(/[^0-9]/g, ''));
}
