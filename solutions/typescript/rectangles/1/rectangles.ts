// given the text imput of ascii rectangles, count the number of rectangles
// input looks like this:
// ['   +--+',
// '  ++  |',
// '+-++--+',
// '|  |  |',
// '+--+--+']
//
// this has 6 rectangles

export function count(lines: string[]): number {
  let rectangles = 0;
  for (let y = 0; y < lines.length; y++) {
    for (let x = 0; x < lines[y].length; x++) {
      if (lines[y][x] !== '+') {
        continue;
      }
      rectangles += findRectangles(lines, x, y);
    }
  }
  return rectangles;
}

function findRectangles(lines: string[], x: number, y: number): number {
  let count = 0;
  for (let height = 1; y + height < lines.length; height++) {
    for (let width = 1; x + width < lines[y].length; width++) {
      if (isRectangle(lines, x, y, width, height)) {
        count++;
      }
    }
  }
  return count;
}

function isRectangle(lines: string[], x: number, y: number, width: number, height: number): boolean {
  if (lines[y][x] != '+' || lines[y + height][x] != '+' || lines[y][x + width] != '+' || lines[y + height][x + width] != '+') {
    return false;
  }
  for (let next_y = y + 1; next_y < y + height; next_y++) {
    if ((lines[next_y][x] != '|' && lines[next_y][x] != '+') || (lines[next_y][x + width] != '|' && lines[next_y][x + width] != '+')) {
      return false;
    }
  }
  for (let next_x = x + 1; next_x < x + width; next_x++) {
    if ((lines[y][next_x] != '-' && lines[y][next_x] != '+') || (lines[y + height][next_x] != '-' && lines[y + height][next_x] != '+')) {
      return false;
    }
  }
  return true;
}
