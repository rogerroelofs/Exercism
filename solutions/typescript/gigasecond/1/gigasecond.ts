export class Gigasecond {
  #input: Date

  constructor(input: Date) {
    this.#input = input;
  }

  public date(): Date {
    let secs: number = this.#input.getTime() / 1000;
    return new Date((secs + 1_000_000_000) * 1000);
  }
}
