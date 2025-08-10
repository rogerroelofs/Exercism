export function isArmstrongNumber(number: number): boolean {
  const digits: string = number.toString().split('');
  const answer: number = digits.reduce((acc: number, cur: string) => {
    return acc + parseInt(cur, 10) ** digits.length;
  }, 0);
  return answer === number;
}
