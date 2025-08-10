export function find(haystack: number[], needle: number): number | never {
  let lo = 0;
  let hi = haystack.length - 1;
  while (lo <= hi) {
    const mid = Math.floor(lo + (hi - lo) / 2);
    if (haystack[mid] === needle) return mid;
    if (haystack[mid] > needle) hi = mid - 1;
    if (haystack[mid] < needle) lo = mid + 1;
  }
  throw new Error('Value not in array');
}
