function assert_original_valid(phone) {
  if ( phone.match(/[A-Za-z]/) ) throw new Error('Letters not permitted');
  if ( phone.match(/[@:!]/) ) throw new Error('Punctuations not permitted');
}

function sanitize_cleaned(cleaned) {
  if ( cleaned.length === 11 && cleaned[0] !== '1' ) throw new Error('11 digits must start with 1');
  if ( cleaned.length === 11 ) cleaned = cleaned.substring(1);
  if ( cleaned.length > 10 ) throw new Error('More than 11 digits');
  if ( cleaned.length !== 10 ) throw new Error('Incorrect number of digits');
  if ( cleaned[0] === '0' ) throw new Error('Area code cannot start with zero');
  if ( cleaned[0] === '1' ) throw new Error('Area code cannot start with one');
  if ( cleaned[3] === '0' ) throw new Error('Exchange code cannot start with zero');
  if ( cleaned[3] === '1' ) throw new Error('Exchange code cannot start with one');
  return cleaned;
}

export const clean = (phone) => {
  assert_original_valid(phone);
  return sanitize_cleaned(phone.replace(/[^0-9]/g, ''));
}