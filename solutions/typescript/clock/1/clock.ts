const CLOCK_MIN = 60;
const CLOCK_HR = 24;
const DAY_MIN = CLOCK_HR * CLOCK_MIN;

function formatNumber(input: number): string {
  return input.toString().padStart(2, '0');
}

export class Clock {
  #minutes = 0;

  constructor(hour: number, minute?: number) {
    const min = (hour * CLOCK_MIN + (minute ?? 0)) % DAY_MIN;
    // this is not a general solution.
    this.#minutes = (min < 0) ? min + DAY_MIN : min;
  }

  public toString(): string {
    return `${formatNumber(Math.trunc(this.#minutes / 60))}:${formatNumber(this.#minutes % 60)}`;
  }

  public plus(minutes: number): Clock {
    return new Clock(0, this.#minutes + minutes);
  }

  public minus(minutes: number): Clock {
    return new Clock(0, this.#minutes - minutes);
  }

  public equals(other: Clock): boolean {
    return other.#minutes === this.#minutes;
  }
}
