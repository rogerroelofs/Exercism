export class BankAccount {
  #balance = 0;
  #status = 'uninitialized';

  open() {
    this.#checkStatus('open');
    this.#balance = 0;
    this.#status = 'open';
  }

  close() {
    this.#checkStatus('close');
    this.#status = 'closed';
  }

  deposit(amt) {
    this.#checkStatus();
    this.#checkAmt(amt);
    this.#balance += amt;
  }

  withdraw(amt) {
    this.#checkStatus();
    this.#checkAmt(amt);
    this.#checkBalance(amt);
    this.#balance -= amt;
  }

  get balance() {
    this.#checkStatus();
    return this.#balance;
  }

  #checkStatus(operation = '') {
    if ((operation == '') && this.#status === 'closed'
      || (operation == 'open' && (! ['uninitialized', 'closed'].includes(this.#status)))
      || (operation == 'close' && this.#status !== 'open')) {
      throw new ValueError();
    }
  }

  #checkBalance(amt) {
    if (this.#balance < amt) {
      throw new ValueError();
    }
  }

  #checkAmt(amt) {
    if (amt < 0) {
      throw new ValueError();
    }
  }
}

export class ValueError extends Error {
  constructor() {
    super('Bank account error');
  }
}
