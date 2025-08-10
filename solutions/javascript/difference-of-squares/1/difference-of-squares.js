export class Squares {
  constructor(count) {
    this.range = Array.from(Array(count).keys()).map(n => n + 1)
  }

  get sumOfSquares() {
    return this.range.reduce((acc, num) => acc + num ** 2);
  }

  get squareOfSum() {
    return this.range.reduce((acc, num) => acc + num) ** 2;
  }

  get difference() {
    return this.squareOfSum - this.sumOfSquares;
  }
}

