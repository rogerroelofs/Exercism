export const eggCount = (displayValue: number): number => {
  return displayValue
    .toString(2)
    .split('')
    .filter((bit) => bit === '1')
    .length;
}
