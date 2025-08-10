export class Triangle {
    constructor(private readonly numRows: number) {}
    get rows(): number[][] {
        const triangle: number[][] = [];
        triangle.push(this.getRow([]));
        for (let i = 1; i < this.numRows; i++) {
            triangle.push(this.getRow(triangle[i - 1]));
        }
        return triangle;
    }
    get lastRow(): number[] {
        return this.rows[this.numRows - 1];
    }
    getRow(prevRow: number[]): number[] {
        if (prevRow.length === 0) {
            return [1];
        }
        const prevZero = [0, ...prevRow, 0];
        const row = [];
        for (let i = 0; i < prevZero.length - 1; i++) {
            row.push(prevZero[i] + prevZero[i + 1]);
        }
        return row;
    }
}
