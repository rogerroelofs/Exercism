function gcd(a: number, b: number): number {
  return b ? gcd(b, a % b) : a;
}

function nthRoot(base: number, n: number): number {
  return Math.pow(base, 1 / n);
}

export class Rational {
  numerator: number;
  denominator: number;

  constructor(numerator: number, denominator: number) {
    if (denominator === 0) {
      throw "Denominator value must be != 0";
    }
    this.numerator = numerator;
    this.denominator = denominator;
    this.reduce();
  }

  add(other: Rational): Rational {
    return new Rational(
      this.numerator * other.denominator + this.denominator * other.numerator,
      this.denominator * other.denominator
    );
  }

  sub(other: Rational): Rational {
    return this.add(new Rational(-other.numerator, other.denominator));
  }

  mul(other: Rational): Rational {
    return new Rational(this.numerator * other.numerator, this.denominator * other.denominator);
  }

  div(other: Rational): Rational {
    return new Rational(this.numerator * other.denominator, this.denominator * other.numerator);
  }

  abs(): Rational {
    return new Rational(Math.abs(this.numerator), Math.abs(this.denominator));
  }

  exprational(exp: number): Rational {
    return exp > 0
      ? new Rational(this.numerator ** exp, this.denominator ** exp)
      : new Rational(this.denominator ** -exp, this.numerator ** -exp);
  }

  expreal(base: number): number {
    return nthRoot(base, this.denominator) ** this.numerator;
  }

  reduce(): Rational {
    const reduced = gcd(this.numerator, this.denominator);
    this.numerator /= reduced;
    this.denominator /= reduced;
    if (this.denominator < 0) {
      this.numerator *= -1;
      this.denominator *= -1;
    }
    return this;
  }
}