export function rotate(clearText: string, key: number): string {
  return clearText
    .split('')
    .map((char) => {
      if (char.match(/[a-z]/i)) {
        const isUpperCase = char === char.toUpperCase();
        const charCode = char.toLowerCase().charCodeAt(0);
        const rotatedCharCode = ((charCode - 97 + key) % 26) + 97;
        return String.fromCharCode(isUpperCase ? rotatedCharCode - 32 : rotatedCharCode);
      }
      return char;
    })
    .join('');
}
