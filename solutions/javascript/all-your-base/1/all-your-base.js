
/** convert: convert a number from one base to another
  * @param {Array} input: an array of digits
  * @param {Number} from_base: the base of the input number
  * @param {Number} to_base: the base of the output number
  * @return {Array} an array of digits representing the output number
  */
export const convert = (input, from_base, to_base) => {
  validate(input, from_base, to_base);
  // if no conversion is needed, just return the input
  if (from_base === to_base) return input;
  // convert the input number to base 10 and then to the output base
  return fromBase10(toBase10(input, from_base), to_base);
};

const validate = (input, from_base, to_base) => {
  if (from_base < 2) {
    throw new Error('Wrong input base');
  }
  if (to_base < 2) {
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

const toBase10 = (input, from_base) => {
  return input.reduce((acc, digit, index) => {
    return acc + digit * Math.pow(from_base, input.length - index - 1);
  }, 0);
};

const fromBase10 = (toBase10, to_base) => {
  let output = [];
  while (toBase10 > 0) {
    output.unshift(toBase10 % to_base);
    toBase10 = Math.floor(toBase10 / to_base);
  }
  return output.length === 0 ? [0] : output;
};
