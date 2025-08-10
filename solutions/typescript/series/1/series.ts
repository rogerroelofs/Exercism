const LENGTH = {
  OVER: new Error('slice length cannot be greater than series length'),
  ZERO: new Error('slice length cannot be zero'),
  NEGATIVE: new Error('slice length cannot be negative'),
};
const SERIES_EMPTY = new Error('series cannot be empty');

export class Series {
  #ar: number[]

  constructor(series: string) {
    this.#ar = [...series].map(v => Number(v))
  }

  slices(sliceLength: number): number[][] {
    this.assert_valid(sliceLength);

    let ret: number[][] = []
    for ( let i = 0; i <= this.#ar.length - sliceLength; i++ ) {
      ret = [...ret, [...this.#ar.slice(i, i + sliceLength)]]
    }

    return ret
  }

  assert_valid(sliceLength: number): void {
    if (this.#ar.length === 0) throw SERIES_EMPTY
    if (sliceLength === 0) throw LENGTH.ZERO
    if (sliceLength < 0) throw LENGTH.NEGATIVE
    if (this.#ar.length < sliceLength) throw LENGTH.OVER
  }
}