export const annotate = (input) => {
  const internal = input.map(row => row.split(''));
  if (input.length === 0) return [];
  if (input[0].length === 0) return [''];
  if (input.length === 1 && input[0].length === 1) return input;
  return input.map((row, i) => {
    return row.split('').map((cell, j) => {
      if (cell === '*') return '*';
      return countMines(internal, i, j);
    }).join('');
  });
};

const countMines = (input, i, j) => {
  let count = 0;
  for (let x = -1; x <= 1; x++) {
    for (let y = -1; y <= 1; y++) {
      if (i + x >= 0 && i + x < input.length && j + y >= 0 && j + y < input[0].length) {
        if (input[i + x][j + y] === '*') count++;
      }
    }
  }
  return count === 0 ? ' ' : count.toString();
}