export class ComplexNumber {
  real: number;
  imag: number;

  constructor(real: number, imag: number) {
    this.real = real;
    this.imag = imag;
  }

  public add(other: ComplexNumber): ComplexNumber {
    return new ComplexNumber(this.real + other.real, this.imag + other.imag);
  }

  public sub(other: ComplexNumber): ComplexNumber {
    return new ComplexNumber(this.real - other.real, this.imag - other.imag);
  }

  public div(other: ComplexNumber): ComplexNumber {
    const denominator = other.real ** 2 + other.imag ** 2;
    return new ComplexNumber(
      (this.real * other.real + this.imag * other.imag) / denominator,
      (this.imag * other.real - this.real * other.imag) / denominator
    );
  }

  public mul(other: ComplexNumber): ComplexNumber {
    return new ComplexNumber(this.real * other.real - this.imag * other.imag, this.real * other.imag + this.imag * other.real);
  }

  public get abs(): number {
    return (this.real ** 2 + this.imag ** 2) ** 0.5;
  }

  public get conj(): ComplexNumber {
    return new ComplexNumber(this.real, this.imag === 0 ? this.imag : -this.imag);
  }

  public get exp(): ComplexNumber {
    return new ComplexNumber(Math.E ** this.real, 0).mul(new ComplexNumber(Math.cos(this.imag), Math.sin(this.imag)));
  }
}