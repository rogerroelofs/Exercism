enum Planet {
  mercury = 'mercury',
  venus = 'venus',
  earth = 'earth',
  mars = 'mars',
  jupiter = 'jupiter',
  saturn = 'saturn',
  uranus = 'uranus',
  neptune = 'neptune',
}

const ORBITS: Record<Planet, number> = {
  mercury: 0.2408467,
  venus: 0.61519726,
  earth: 1.0,
  mars: 1.8808158,
  jupiter: 11.862615,
  saturn: 29.447498,
  uranus: 84.016846,
  neptune: 164.79132,
}
const EARTH_SECONDS = 31557600

export function age(planet: string, seconds: number): number {
  const planet_years = seconds / (EARTH_SECONDS * ORBITS[planet as Planet])
  return Number.parseFloat(planet_years.toFixed(2))
}
