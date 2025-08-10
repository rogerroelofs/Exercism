export const rotate = (clearText, key) => {
  return clearText
    .split('')
    .map((char) => {
      if (char.match(/[a-z]/i)) {
        const isLowerCase = char === char.toLowerCase();
        const charCode = char.toLowerCase().charCodeAt(0);
        const rotatedCharCode = ((charCode - 97 + key) % 26) + 97;
        return String.fromCharCode(isLowerCase ? rotatedCharCode : rotatedCharCode - 32);
      }
      return char;
    })
    .join('');
};
