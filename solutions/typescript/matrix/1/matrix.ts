export class Matrix {
  _rows: number[][]

  constructor(input: string) {
    this._rows = (input.split("\n")
    .map((row: string): number[] => (
      row.split(" ").map((cell: string): number => parseInt(cell))
    )));
  }

  get rows(): number[][] {
    return this._rows;
  }

  get columns(): number[][] {
    return this.transpose();
  }

  transpose(): number[][] {
    return this._rows[0].map((_, c) => this._rows.map(r => r[c]));
  }
}
