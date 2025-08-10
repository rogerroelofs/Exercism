const COLORS = "black brown red orange yellow green blue violet grey white".split(" ")

export function decodedResistorValue(colors: string[]) : string {
  const [first, second, third] = colors
  return encode(decodeTwo(first, second) * calcMagnitude(third))
}

function decodeTwo(first:string, second:string) : number {
  return COLORS.indexOf(first) * 10 + COLORS.indexOf(second)
}

function calcMagnitude(third:string) : number {
  return 10 ** COLORS.indexOf(third)
}

function encode(total:number) : string {
  if ( total >= 1000) {
    return `${total/1000} kiloohms`
  }
  return `${total} ohms`
}
