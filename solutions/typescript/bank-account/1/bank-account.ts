//
// This is only a SKELETON file for the 'Bank Account' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export class ValueError extends Error {
  constructor() {
    super('Bank account error')
  }
}

export class BankAccount {
  #balance: number = 0;
  #status: string = 'uninitialized';

  open(): void {
    this.#checkStatus('open');
    this.#balance = 0;
    this.#status = 'open';
  }

  close(): void {
    this.#checkStatus('close');
    this.#status = 'closed';
  }

  deposit(amt: number): void {
    this.#checkStatus();
    this.#checkAmt(amt);
    this.#balance += amt;
  }

  withdraw(amt: number): void {
    this.#checkStatus();
    this.#checkAmt(amt);
    this.#checkBalance(amt);
    this.#balance -= amt;
  }

  get balance(): number {
    this.#checkStatus();
    return this.#balance;
  }

  #checkStatus(operation: string = ''): void {
    if ((operation == '') && this.#status === 'closed'
      || (operation == 'open' && (! ['uninitialized', 'closed'].includes(this.#status)))
      || (operation == 'close' && this.#status !== 'open')) {
      throw new ValueError();
    }
  }

  #checkBalance(amt: number): void {
    if (this.#balance < amt) {
      throw new ValueError();
    }
  }

  #checkAmt(amt: number): void {
    if (amt < 0) {
      throw new ValueError();
    }
  }
}
