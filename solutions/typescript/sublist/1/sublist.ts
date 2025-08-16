export class List {
  #list: number[];
  constructor(...list: number[]) {
    this.#list = list.length > 0 ? list : [];
  }

  compare(other: List): string {
    const myList = this.#list;
    const otherList = other.#list;

    // Helper to check if arr1 is a contiguous subarray of arr2
    function isSublist(arr1: number[], arr2: number[]): boolean {
      if (arr1.length === 0) return true;
      if (arr1.length > arr2.length) return false;
      for (let i = 0; i <= arr2.length - arr1.length; i++) {
        if (arr2.slice(i, i + arr1.length).every((val, idx) => val === arr1[idx])) {
          return true;
        }
      }
      return false;
    }

    if (myList.length === otherList.length && isSublist(myList, otherList)) {
      return 'equal';
    } else if (isSublist(myList, otherList)) {
      return 'sublist';
    } else if (isSublist(otherList, myList)) {
      return 'superlist';
    }
    return 'unequal';
  }
}
