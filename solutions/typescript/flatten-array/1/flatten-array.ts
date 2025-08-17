type NestedNumberArray = Array<number | undefined | NestedNumberArray>;

export function flatten(array: NestedNumberArray): number[] {
  const result: number[] = [];
  for (const item of array) {
    if (Array.isArray(item)) {
      result.push(...flatten(item));
    } else if (typeof item === 'number') {
      result.push(item);
    }
  }
  return result;
}
