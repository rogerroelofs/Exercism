export function ofSize(dim: number): number[][] | [] {
  if (dim < 1) return [];
  // create a matrix of size dim x dim
  const matrix = Array.from({ length: dim }, () => Array(dim).fill(0));
  let counter = 1;
  let [startRow, startCol] = [0, 0];
  let endRow = dim - 1;
  let endCol = endRow;
  // populate a matrix in spiral order
  while (startRow <= endRow && startCol <= endCol) {
    // top row
    for (let i = startCol; i <= endCol; i++) {
      matrix[startRow][i] = counter++;
    }
    startRow++;
    // right column
    for (let i = startRow; i <= endRow; i++) {
      matrix[i][endCol] = counter++;
    }
    endCol--;
    // bottom row
    for (let i = endCol; i >= startCol; i--) {
      matrix[endRow][i] = counter++;
    }
    endRow--;
    // left column
    for (let i = endRow; i >= startRow; i--) {
      matrix[i][startCol] = counter++;
    }
    startCol++;
  }
  return matrix;
}
