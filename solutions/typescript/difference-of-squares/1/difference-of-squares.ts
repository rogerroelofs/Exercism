export class Squares {
  #range: number[]

  constructor(count: number) {
    this.#range = Array.from(Array(count).keys()).map(n => n + 1)
  }

  get sumOfSquares(): number {
    return this.#range.reduce((acc: number, num: number): number => acc + num ** 2);
  }

  get squareOfSum(): number {
    return this.#range.reduce((acc: number, num: number): number => acc + num) ** 2;
  }

  get difference(): number {
    return this.squareOfSum - this.sumOfSquares;
  }
}
