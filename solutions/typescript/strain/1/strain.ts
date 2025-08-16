export function keep<T>(input: T[], predicate: (value: T) => boolean): T[] {
  return input.filter(predicate);
}

export function discard<T>(input: T[], predicate: (value: T) => boolean): T[] {
  return input.filter(value => !predicate(value));
}
