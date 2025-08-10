type Player = 'O' | 'X';
const adjacent = [[1, 0], [-1, 0], [0, -1], [1, -1], [-1, 1], [0, 1]];

export class Board {
  constructor(private board: string[]) {}

  public winner(): string {
    const fields = this.board.map((_, i) => i);
    if (fields.some(x => this.path(x, 0, 'O'))) return 'O';
    if (fields.some(y => this.path(0, y, 'X'))) return 'X';
    return '';
  }

  private path(x: number, y: number, player: Player, traveled = new Set()): boolean {
    if (this.board[y]?.[x * 2 + y] !== player || 
      traveled.has(y * this.board.length + x)) return false;
    if (player === 'O' && (y + 1 === this.board.length) ||
      player === 'X' && (x * 2 + y + 1 === this.board[y].length)) return true;
    traveled.add(y * this.board.length + x);
    return adjacent.some(([dx, dy]) => this.path(x + dx, y + dy, player, traveled));
  }
}