class Item<TElement>{
  public value: TElement;
  public next: Item<TElement> | null = null;
  public prev: Item<TElement> | null = null;

  constructor(value:TElement){
    this.value = value;
  }
}

export class LinkedList<TElement> {
  private first: Item<TElement> | null = null;
  private last: Item<TElement> | null = null;

  public push(element: TElement): void {
    const item = new Item(element);
    if ( this.last === null ) {
      this.first = item;
      this.last = item;
    } else {
      this.last.next = item;
      item.prev = this.last;
      this.last = item;
    }
  }

  public pop(): TElement | null {
    if ( this.last === null ) return null;
    const ret = this.last?.value;
    const newLast = this.last?.prev;
    if ( newLast !== null ) {
      newLast.next = null;
      this.last = newLast;
    } else {
      this.last = null;
      this.first = null;
    }

    return ret;
  }

  public shift(): TElement | null {
    if ( this.first === null ) return null;
    const ret = this.first.value;
    const newFirst = this.first.next;
    if ( newFirst !== null ) {
      newFirst.prev = null;
      this.first = newFirst;
    } else {
      this.first = null;
      this.last = null;
    }

    return ret;
  }

  public unshift(element: TElement): void {
    const item = new Item(element);
    if ( this.first === null ) {
      this.last = item;
      this.first = item;
    } else {
      this.first.prev = item;
      item.next = this.last;
      this.first = item;
    }
  }

  public delete(element: TElement): void {
    // empty
    if ( this.first === null ) return;

    // one item and match
    var item = this.first;
    if ( this.last === this.first && item.value === element ) {
      this.first = null;
      this.last = null;
    }

    while ( item.next ) {
      if ( item.value === element ) {
        const prev = item.prev;
        const next = item.next;
        item.prev = null;
        item.next = null;
        if ( prev !== null ) {
          prev.next = next;
        } else {
          this.first = next;
        }
        if ( next !== null ) {
          next.prev = next;
        } else {
          this.last = next;
        }
        break;
      }
      item = item.next;
    }
  }

  public count(): number {
    if ( this.first === null ) return 0;

    let count = 1;
    var item = this.first;
    while ( item.next ) {
      count++;
      item = item.next;
    }

    return count;
  }
}
