const valid = 'abcdefghijklmnopqrstuvwxyz'

export function isPangram(input:string) : boolean {
  if (input === '') return false
  return (input
    .toLocaleLowerCase()
    .replace(/[^a-z]/g, '')
    .split('')
    .filter(unique)
    .sort()
    .join('') === valid)
}

function unique(value:string, index:number, self:string[]) : boolean {
  return self.indexOf(value) === index;
}