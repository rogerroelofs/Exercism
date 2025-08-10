export function convert(
  digits: number[],
  inputBase: number,
  outputBase: number
): number[] {
  validate(digits, inputBase, outputBase);
  // if no conversion is needed, just return the input
  if (inputBase === outputBase) return digits;
  // convert the input number to base 10 and then to the output base
  return fromBase10(toBase10(digits, inputBase), outputBase);
}

const validate = (input: number[], from_base: number, to_base: number): void => {
  if ((from_base < 2) || (!Number.isInteger(from_base))) {
    throw new Error('Wrong input base');
  }
  if ((to_base < 2) || (!Number.isInteger(to_base))) {
    throw new Error('Wrong output base');
  }
  if (
    (input.length === 0) ||
    (input.some(digit => digit < 0)) ||
    (input.some(digit => digit >= from_base)) ||
    (input.length > 1 && input[0] === 0)
  ) {
    throw new Error('Input has wrong format');
  }
};

const toBase10 = (input: number[], from_base: number): number => {
  return input.reduce((acc, digit, index) => {
    return acc + digit * Math.pow(from_base, input.length - index - 1);
  }, 0);
};

const fromBase10 = (input: number, to_base: number): number[] => {
  let output = [];
  while (input > 0) {
    output.unshift(input % to_base);
    input = Math.floor(input / to_base);
  }
  return output.length === 0 ? [0] : output;
};
