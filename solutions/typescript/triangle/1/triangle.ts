export class Triangle {
  private sides: number[]

  constructor(...sides: number[]) {
    this.sides = sides.sort();
  }

  get isEquilateral(): boolean {
    if ( ! this.isValid() ) return false
    return this.sides[0] === this.sides[1] && this.sides[1] == this.sides[2];
  }

  get isIsosceles(): boolean {
    if ( ! this.isValid() ) return false
    return this.sides[0] === this.sides[1] || this.sides[1] == this.sides[2];
  }

  get isScalene(): boolean {
    if ( ! this.isValid() ) return false
    return ! this.isIsosceles && ! this.isEquilateral;
  }

  private isValid(): boolean {
    return (this.sides.length === 3) && (this.sides[0] > 0) && (this.sides[0] + this.sides[1] >= this.sides[2]);
  }
}
