export const find = (haystack, needle) => {
  let left = 0;
  let right = haystack.length - 1;
  while (left <= right) {
    let middle = Math.floor((left + right) / 2);
    if (haystack[middle] < needle) {
      left = middle + 1;
    } else {
      right = middle - 1;
    }
    if (haystack[middle] === needle) return middle;
  }
  throw new Error('Value not in array');
};
