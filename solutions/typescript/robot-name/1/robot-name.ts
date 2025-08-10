
function random(max: number): number {
  return Math.floor(Math.random() * max);
}

function calcLetters(): string {
  const start = 'A'.charCodeAt(0);
  return String.fromCharCode(start + random(26), start + random(26));
}

function calcNumbers(): string {
  return [random(10), random(10), random(10)].join('');
}


export class Robot {
  #name = '';
  static names = new Set()

  constructor() {
    this.#name = this.calcName();
  }

  calcName(): string {
    let name = '';
    do {
      name = `${calcLetters()}${calcNumbers()}`;
    } while (Robot.names.has(name))

    Robot.names.add(name);

    return name;
  }

  get name(): string {
    return this.#name;
  }

  resetName(): void {
    this.#name = this.calcName();
  }

  static releaseNames(): void {
    Robot.names = new Set();
  }
}
