type Position = readonly [number, number]

type Positions = {
  white: Position
  black: Position
}
export class QueenAttack {
  public readonly black: Position
  public readonly white: Position

  // white: [whiteRow, whiteColumn]
  // black: [blackRow, blackColumn]
  constructor(positions: Partial<Positions> = {}) {
    this.validatePositions(positions);
    this.white = positions.white ?? [7, 3]
    this.black = positions.black ?? [0, 3]
    if (
      this.white[0] === this.black[0] &&
      this.white[1] === this.black[1]
    ) {
      throw new Error('Queens cannot share the same space')
    }
  }

  toString() {
    let board = [
        '_ _ _ _ _ _ _ _',
        '_ _ _ _ _ _ _ _',
        '_ _ _ _ _ _ _ _',
        '_ _ _ _ _ _ _ _',
        '_ _ _ _ _ _ _ _',
        '_ _ _ _ _ _ _ _',
        '_ _ _ _ _ _ _ _',
        '_ _ _ _ _ _ _ _',
      ];
      const colPos = (col: number) => col * 2;
      let row = board[this.white[0]].split('');
      row[colPos(this.white[1])] = 'W';
      board[this.white[0]] = row.join('');
      row = board[this.black[0]].split('');
      row[colPos(this.black[1])] = 'B';
      board[this.black[0]] = row.join('');
      return board.join('\n');
  }

  get canAttack() {
    if (this.white[0] === this.black[0]) return true; // same row
    if (this.white[1] === this.black[1]) return true; // same column
    if (Math.abs(this.white[0] - this.black[0]) === Math.abs(this.white[1] - this.black[1])) return true; // diagonal attack
    return false;
  }

  validatePositions(positions: Partial<Positions>) {
    Object.entries(positions).forEach(([color, pos]) => {
      if (!pos) {
        throw new Error(`Queen must be placed on the board`)
      }
      const [row, col] = pos;
      if (row < 0 || row > 7 || col < 0 || col > 7) {
        throw new Error('Queen must be placed on the board')
      }
    });
  }
}
