
const DIGIT_MAP = {
  '0': ' _ \n| |\n|_|\n   ',
  '1': '   \n  |\n  |\n   ',
  '2': ' _ \n _|\n|_ \n   ',
  '3': ' _ \n _|\n _|\n   ',
  '4': '   \n|_|\n  |\n   ',
  '5': ' _ \n|_ \n _|\n   ',
  '6': ' _ \n|_ \n|_|\n   ',
  '7': ' _ \n  |\n  |\n   ',
  '8': ' _ \n|_|\n|_|\n   ',
  '9': ' _ \n|_|\n _|\n   ',
}

export function convert(grid: string) {
  /* 
Given a 3 x 4 grid of pipes, underscores, and spaces, determine which number is represented, or whether it is garbled.
*/
  /* 
  split the grid into 3x4 chunks
  */
  // Split into rows
  const rows = grid.split('\n')
  
  // Calculate number of digits (each is 3 chars wide)
  const numDigits = rows[0].length / 3

  // For each group of 4 lines
  const groups: string[][] = [];
  for (let group = 0; group < rows.length; group += 4) {
    const groupChunks: string[] = [];
    // For each digit position in this group
    for (let i = 0; i < numDigits; i++) {
      const digit = [
        rows[group].slice(i * 3, (i + 1) * 3),
        rows[group + 1].slice(i * 3, (i + 1) * 3), 
        rows[group + 2].slice(i * 3, (i + 1) * 3),
        rows[group + 3].slice(i * 3, (i + 1) * 3)
      ].join('\n');
      groupChunks.push(digit);
    }
    groups.push(groupChunks);
  }

  const ocr_digits = Object.values(DIGIT_MAP);
  const ans_digits = Object.keys(DIGIT_MAP);
  return groups.map(line => {
    return line.map(digit => {
      const index = ocr_digits.findIndex(d => d === digit);
      if (index === -1) return '?';
      return ans_digits[index];
    }).join('');
  }).join(',');
}
