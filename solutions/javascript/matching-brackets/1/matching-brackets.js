const openBrackets = ['(', '[', '{'];
const closeBrackets = [')', ']', '}'];

export const isPaired = (input) => {
  const stack = [];
  for (const char of input) {
    if (openBrackets.includes(char)) {
      stack.push(char);
    } else if (closeBrackets.includes(char)) {
      const openBracket = stack.pop();
      if (openBracket === undefined) {
        return false;
      }
      const openBracketIndex = openBrackets.indexOf(openBracket);
      if (closeBrackets[openBracketIndex] !== char) {
        return false;
      }
    }
  }

  if (stack.length > 0) {
    return false;
  }

  return true;
};
