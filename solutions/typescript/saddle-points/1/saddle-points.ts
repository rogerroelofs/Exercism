/* 
  Given a grid of sy coordinates find the position where the candidate
  is the smallest in the column and the largest in the row
  */

export function saddlePoints(grid: number[][]): { row: number, column: number }[] {
  if (grid.length === 0) return [];

  const result: { row: number; column: number }[] = [];
  
  // For each position in the grid
  for (let row = 0; row < grid.length; row++) {
    for (let col = 0; col < grid[0].length; col++) {
      const candidate = grid[row][col];
      
      // Check if largest in row
      const isLargestInRow = grid[row].every(num => num <= candidate);
      
      // Check if smallest in column
      const isSmallestInColumn = grid.every(r => r[col] >= candidate);
      
      if (isLargestInRow && isSmallestInColumn) {
        result.push({ row: row + 1, column: col + 1 }); // 1-based indices
      }
    }
  }

  return result;
}
