const normalize = (str) => str.toLowerCase().split('').sort().join('');

export const findAnagrams = (word, candidates) => {
  const cmp = normalize(word);
  word = word.toLowerCase();
  return candidates.filter((str) => {
    if ( str.toLowerCase() === word ) return false;
    return cmp === normalize(str);
  });
};
