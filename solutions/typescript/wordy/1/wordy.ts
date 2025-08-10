type Ops = 'plus' | 'minus' | 'multiplied' | 'divided' | 'raised'
type MathOps = {
  [key: string]: (a: number, b: number) => number
}
type Receiver = {
  terms: number[],
  ops: Ops[],
  current: 'terms' | 'ops'
}

const isOp = (op: string): boolean => {
  return ['plus', 'minus', 'multiplied', 'divided', 'raised'].includes(op);
}

const mathOps: MathOps = {
  plus: (a: number, b: number): number => a + b,
  minus: (a: number, b: number): number => a - b,
  multiplied: (a: number, b: number): number => a * b,
  divided: (a: number, b: number): number => a / b,
  raised: (a: number, b: number): number => a ** b,
};

function parse_tokens(input: string[]): [number[], Ops[]] {
  const receiver: Receiver = {
    terms: [],
    ops: [],
    current: 'terms'
  };
  input.forEach((word: string): void => {
    if (receiver.current === 'terms') {
      receiver.terms.push(parseInt(word));
    } else {
      const op: Ops | undefined = (isOp(word)) ? word as Ops : undefined;
      if ( op ) {
        receiver.ops.push(op);
      } else {
        if ( parseInt(word, 10) > 0 ) {
          throw('Syntax error');
        }
        throw("Unknown operation")
      }
    }
    receiver.current = (receiver.current === 'terms') ? 'ops' : 'terms';
  });
  return [receiver.terms, receiver.ops];
}

export const answer = (input: string): number => {
  const words = input.replace(/\?/, '').split(' ').filter((word: string): boolean => {
    return (["What", "is", 'by'].indexOf(word) === -1);
  });

  let terms: number[] = [];
  let ops: string[] = [];
  [terms, ops] = parse_tokens(words);
  while ( terms.length > 1 ) {
    terms[0] = mathOps[ops[0]](terms[0], terms[1])
    terms.splice(1, 1);
    ops.splice(0, 1);
  }
  if (( ops.length > 0 ) || (terms.length !== 1)) {
    throw('Syntax error');
  }
  return terms[0];
}
