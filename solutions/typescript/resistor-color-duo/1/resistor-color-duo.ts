const COLORS = "black brown red orange yellow green blue violet grey white".split(" ")

export function decodedValue(colors: string[]) :number {
  const [first, second] = colors
  return (COLORS.indexOf(first) * 10 + COLORS.indexOf(second))
}
