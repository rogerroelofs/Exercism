export const COLORS:string[] = "black brown red orange yellow green blue violet grey white".split(" ")

export const colorCode = (color:string) : number => {
  return COLORS.indexOf(color)
}

