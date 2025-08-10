export default class CircularBuffer<T> {
  private buffer: T[];
  private head: number;
  private tail: number;
  private count: number;

  constructor(length: number) {
    this.buffer = Array.from(Array(length));
    this.head = this.tail = this.count = 0;
  }

  write(value: T): void {
    if ( this.count >= this.buffer.length ) {
      throw new BufferFullError();
    }
    this.buffer[this.head] = value;
    this.head = (this.head + 1) % this.buffer.length;
    this.count++;
  }

  read(): T {
    if ( this.count === 0 ) {
      throw new BufferEmptyError();
    }
    let value = this.buffer[this.tail];
    this.tail = (this.tail + 1) % this.buffer.length;
    this.count--;
    return value;
  }

  forceWrite(value: T): void {
    if ( this.count >= this.buffer.length ) {
      this.read();
    }
    this.write(value);
  }

  clear(): void {
    this.head = this.tail = this.count = 0;
  }
}

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
