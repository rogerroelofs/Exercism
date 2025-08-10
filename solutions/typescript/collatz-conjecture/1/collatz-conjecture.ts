export function steps(count: number): number {
  let step_count = 0;

  if ( count <= 0 ) throw('Only positive numbers are allowed')

  while(count > 1) {
    count = (count % 2 === 0) ? count / 2 : count * 3 + 1;
    step_count++;
  }

  return step_count;
}
