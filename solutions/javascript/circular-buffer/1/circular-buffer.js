//
// This is only a SKELETON file for the 'Circular Buffer' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

class CircularBuffer {
  constructor(length) {
    this.buffer = Array.from(Array(length));
    this.clear();
  }

  write(value) {
    if ( this.count >= this.buffer.length ) {
      throw new BufferFullError();
    }
    this.buffer[this.head] = value;
    this.head = (this.head + 1) % this.buffer.length;
    this.count++;
  }

  read() {
    if ( this.count === 0 ) {
      throw new BufferEmptyError();
    }
    let value = this.buffer[this.tail];
    this.tail = (this.tail + 1) % this.buffer.length;
    this.count--;
    return value;
  }

  forceWrite(value) {
    if ( this.count >= this.buffer.length ) {
      this.read();
    }
    this.write(value);
  }

  clear() {
    this.head = this.tail = this.count = 0;
  }
}

export default CircularBuffer;

export class BufferFullError extends Error {
  constructor() {
    super("Full");
  }
}

export class BufferEmptyError extends Error {
  constructor() {
    super("Empty");
  }
}
