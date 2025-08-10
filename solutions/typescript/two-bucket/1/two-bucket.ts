type BucketName = 'one' | 'two';
interface Bucket {
  name: BucketName,
  size: number,
  current: number
}

const gcd = (a: number, b: number): number => (b ? gcd(b, a % b) : a)

export class TwoBucket {
  buckets: [Bucket, Bucket];
  goalAmt: number;

  constructor(b1Amt: number, b2Amt: number, goal: number, startBucket: BucketName) {
    this.goalAmt = goal;
    this.buckets = [
      {name: 'one', size: b1Amt, current: 0},
      {name: 'two', size: b2Amt, current: 0}];
    if ( startBucket === 'two' ) {
      this.buckets.reverse();
    }
  }

  moves(): number {
    this.assertIsValid();

    let moves = 0

    while(this.buckets[0].current !== this.goalAmt && this.buckets[1].current !== this.goalAmt) {
      moves += 1

      if(this.buckets[0].current === 0) this.buckets[0].current = this.buckets[0].size
      else if (this.buckets[1].current === this.buckets[1].size) this.buckets[1].current = 0
      else {
        const toTransfer = Math.min(this.buckets[1].size - this.buckets[1].current, this.buckets[0].current)
        this.buckets[0].current -= toTransfer
        this.buckets[1].current += toTransfer
      }
    }

    return moves;
  }

  get goalBucket(): string {
    return this.buckets[0].current === this.goalAmt ? this.buckets[0].name : this.buckets[1].name;
  }

  get otherBucket(): number {
    return this.buckets[0].current === this.goalAmt ? this.buckets[1].current : this.buckets[0].current;
  }

  private assertIsValid(): void {
    if(this.goalAmt > this.buckets[0].size && this.goalAmt > this.buckets[1].size) throw new Error('goal unreachable')

    if (this.goalAmt % gcd(this.buckets[0].size, this.buckets[1].size) !== 0) {
      throw new Error('goal unreachable')
    }
  }
}
