//
// This is only a SKELETON file for the 'Armstrong Numbers' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const isArmstrongNumber = (number) => {
  const digits = number.toString().split('');
  const answer = digits.reduce((acc, cur) => {
    return acc + parseInt(cur, 10) ** digits.length;
  }, 0);
  return answer === number;
};
