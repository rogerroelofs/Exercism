// Description: Given a list of words, compute the number of times each letter is found in all words.
export const parallelLetterFrequency = (texts) => {
  let result = {};
  // prep texts for letter counting
  let formatedTexts = texts
    .map((x) => x.toLowerCase().match(/\p{Letter}+/gu) ?? [])
    .flat();
  Promise.all(formatedTexts.map((text) => processOne(text, result)));
  return result;
};

const processOne = (text, result) => {
  return new Promise((resolve) => {
    let res = text.split('').reduce((acc, cur) => {
      acc[cur] = (acc[cur] || 0) + 1;
      return acc;
    }, result);
    resolve(res);
  });
};