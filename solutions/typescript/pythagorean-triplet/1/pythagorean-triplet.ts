type Options = {
  minFactor?: number
  maxFactor?: number
  sum: number
}

class Triplet {
  #a: number;
  #b: number;
  #c: number;

  constructor(a: number, b: number, c: number) {
    this.#a = a;
    this.#b = b;
    this.#c = c;
  }

  toArray = (): [number, number, number] => [this.#a, this.#b, this.#c];
}

export function triplets(options: Options): Triplet[] {
  const t: Triplet[] = [];
  const max: number = options.maxFactor || options.sum - 1;
  for (let i = options.minFactor || 0; i <= max; i++) {
      for (let j = i + 1; j <= max; j++) {
          const k: number = options.sum - i - j;
          if (k <= j || k > max) continue;
          if (Math.pow(i, 2) + Math.pow(j, 2) === Math.pow(k, 2)) t.push(new Triplet(i, j, k));
      }
  }
  return t;
}
