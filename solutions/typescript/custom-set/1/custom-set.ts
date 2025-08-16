export class CustomSet {
  #data: number[];
  constructor(initial?: number[]) {
    this.#data = initial ?? [];
  }

  empty(): boolean {
    return this.#data.length === 0;
  }

  contains(element: number): boolean {
    return this.#data.includes(element);
  }

  add(element: number): CustomSet {
    if (!this.contains(element)) {
      this.#data.push(element);
    }
    return this;
  }

  subset(other: CustomSet): boolean {
    return this.#data.every(element => other.contains(element));
  }

  disjoint(other: CustomSet): boolean {
    return this.#data.every(element => !other.contains(element));
  }

  eql(other: unknown): boolean {
    if (!(other instanceof CustomSet)) return false;
    return this.#data.length === other.#data.length && this.#data.every(element => other.contains(element));
  }

  union(other: CustomSet): CustomSet {
    const unionSet = new CustomSet([...this.#data]);
    for (const element of other.#data) {
      unionSet.add(element);
    }
    return unionSet;
  }

  intersection(other: CustomSet): CustomSet {
    const intersectionSet = new CustomSet();
    for (const element of this.#data) {
      if (other.contains(element)) {
        intersectionSet.add(element);
      }
    }
    return intersectionSet;
  }

  difference(other: CustomSet): CustomSet {
    const differenceSet = new CustomSet();
    for (const element of this.#data) {
      if (!other.contains(element)) {
        differenceSet.add(element);
      }
    }
    return differenceSet;
  }
}
