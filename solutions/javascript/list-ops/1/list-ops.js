//
// This is only a SKELETON file for the 'List Ops' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export class List {
  constructor(list) {
    this.values = list || [];
  }

  append(list) {
    this.values.push(...list.values);
    return this;
  }

  concat(lists) {
    lists.values.forEach(list => this.append(list));
    return this;
  }

  filter(func) {
    const filtered = [];
    this.values.forEach(item => {
      if ( func(item) ) filtered.push(item);
    })
    this.values = filtered;
    return this;
  }

  map(func) {
    const mapped = [];
    this.values.forEach(item => {
      mapped.push(func(item));
    })
    this.values = mapped;
    return this;
  }

  length() {
    let count = 0;
    this.values.forEach(item => {
      count++;
    });
    return count;
  }

  foldl(func, start) {
    let ret = start;
    this.values.forEach(item => {
      ret = func(ret, item);
    });
    return ret;
  }

  foldr(func, start) {
    let ret = start;
    for ( let i = this.length() - 1; i >= 0; i-- ) {
      ret = func(ret, this.values[i]);
    }
    return ret;
  }

  reverse() {
    const reversed = [];
    for ( let i = this.length() - 1; i >= 0; i-- ) {
      reversed.push(this.values[i]);
    }
    this.values = reversed;
    return this;
  }
}
