export class GameOfLife {
  #gameState: number[][] = [[]];

  constructor(matrix: number[][]) {
    this.#gameState = matrix;
  }

  public tick(): void {
    // Apply the rules of the Game of Life to the current state
    const newState = this.#gameState.map((row: number[]) => [...row]);
    for (let i = 0; i < this.#gameState.length; i++) {
      for (let j = 0; j < this.#gameState[i].length; j++) {
        const alive = this.#gameState[i][j] === 1;
        const neighbors = this.countNeighbors(i, j);
        if (alive && (neighbors < 2 || neighbors > 3)) {
          newState[i][j] = 0;
        } else if (!alive && neighbors === 3) {
          newState[i][j] = 1;
        }
      }
    }
    this.#gameState = newState;
  }

  public state(): number[][] {
    return this.#gameState;
  }

  private countNeighbors(i: number, j: number): number {
    const maxRow = this.#gameState.length;
    const maxCol = this.#gameState[0].length;
    return [-1, 0, 1]
      .flatMap(dx => [-1, 0, 1].map(dy => [dx, dy]))
      .filter(([dx, dy]) => !(dx === 0 && dy === 0))
      .filter(([dx, dy]) => {
        const x = i + dx, y = j + dy;
        return x >= 0 && x < maxRow && y >= 0 && y < maxCol;
      })
      .reduce((sum, [dx, dy]) => sum + this.#gameState[i + dx][j + dy], 0);
  }
}
