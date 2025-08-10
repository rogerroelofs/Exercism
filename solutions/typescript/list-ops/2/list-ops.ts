class Node<T> {
  value: T;
  next: Node<T> | undefined;
  constructor(value: T) {
    this.value = value;
  }
}
/**
 * An implementation of a basic list using core language features
 */
export class List<T> {
  private root: Node<T> | undefined;
  public static create<T>(...values: T[]): List<T | unknown> {
    const list = new List<T | unknown>();
    for (const v of values) {
      list.push(v);
    }
    return list;
  }
  /**
   * Adds an item to the end of the list
   */
  push(element: T): void {
    const item = new Node(element)
    let tip = this.tip();
    if ( tip === undefined ) {
      this.root = item;
    } else {
      tip.next = item;
    }
    return;
  }
  /**
   * Removes an item from the end of the list.
   */
  pop(): T | undefined {
    let tip = this.tip();
    if ( this.root === undefined ) {
      return undefined;
    } else if ( this.root === tip ) { // 1 item in list
      const lastItem = this.root;
      this.root = undefined;
      return lastItem.value;
    }
    let tip2 = this.root;
    while (tip2?.next?.next !== undefined) {
      tip2 = tip2.next;
    }
    const lastItem = tip2?.next;
    tip2.next = undefined;
    return lastItem?.value;
  }
  concat(list: List<T>): List<T> {
    list.forEach(item => {
      if (item instanceof List) {
        this.concat(item);
      }
      else {
        this.push(item);
      }
    });
    return this;
  }
  forEach(callback: (el: T) => void): void {
    let item = this.root;
    while (item !== undefined) {
      callback(item.value);
      item = item.next;
    }
  }
  filter<U extends T>(predicate: (el: U) => boolean): List<U> {
    const result = new List<U>();
    this.forEach(item => {
      if (predicate(item as U))
        result.push(item as U)
    });
    return result;
  }
  map<U extends T>(callback: (el: U) => U): List<U> {
    const result = new List<U>();
    this.forEach(item => result.push(callback(item as U)));
    return result;
  }
  append(list: List<T>): List<T> {
    list.forEach((element) => this.push(element));
    return this;
  }
  foldl<U extends T, V extends T>(callback: (acc: V, el: U) => V, initialValue: V): V {
    let sum = initialValue;
    this.forEach(element => sum = callback(sum, element as U));
    return sum;
  }
  foldr<U extends T, V extends T>(callback: (acc: V, el: U) => V, initialValue: V): V {
    this.reverse();
    const result = this.foldl(callback, initialValue);
    this.reverse();
    return result;
  }
  reverse(): List<T> {
    const stack = new List<T>();
    this.forEach(item => stack.push(item));
    this.root = undefined;
    let item = stack.pop();
    while (item !== undefined) {
      this.push(item);
      item = stack.pop();
    }
    return this;
  }
  length(): number {
    let i = 0;
    let item = this.root;
    while (item !== undefined) {
      item = item.next;
      i++;
    }
    return i;
  }
  private tip(): Node<T> | undefined {
    let currentLastItem = this.root;
    while (currentLastItem?.next !== undefined) {
      currentLastItem = currentLastItem.next;
    }
    return currentLastItem;
  }
}