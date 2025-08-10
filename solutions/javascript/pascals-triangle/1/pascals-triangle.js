export const rows = (numRows) => {
  if (numRows === 0) return [];

  const triangle = [];
  triangle.push(getRow([]));
  for (let i = 1; i < numRows; i++) {
      triangle.push(getRow(triangle[i - 1]));
  }
  return triangle;
};

const getRow = (prevRow) => {
  if (prevRow.length === 0) return [1];

  const prevWithZero = [0, ...prevRow, 0];
  const row = [];
  for (let i = 0; i < prevWithZero.length - 1; i++) {
    row.push(prevWithZero[i] + prevWithZero[i + 1]);
  }
  return row;
}
