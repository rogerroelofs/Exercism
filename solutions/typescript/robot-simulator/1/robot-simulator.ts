export class InvalidInputError extends Error {
  constructor(message: string) {
    super()
    this.message = message || 'Invalid Input'
  }
}

type Direction = 'north' | 'east' | 'south' | 'west'
type Coordinates = [number, number]

export class Robot {
  #bearing: Direction = 'north'
  #coordinates: Coordinates = [0, 0]

  get bearing(): Direction {
    return this.#bearing
  }

  get coordinates(): Coordinates {
    return this.#coordinates
  }

  place({direction, x, y}: { x: number; y: number; direction: string }) {
    if (!['north', 'east', 'south', 'west'].includes(direction)) {
      throw new InvalidInputError('Invalid Input')
    }
    this.#bearing = direction as Direction
    this.#coordinates = [x, y]
  }

  evaluate(instructions: string) {
    for (const instruction of instructions) {
      switch (instruction) {
        case 'L':
          this.turnLeft()
          break
        case 'R':
          this.turnRight()
          break
        case 'A':
          this.advance()
          break
      }
    }
  }

  turnRight: () => void = () => {
    const dirs = ['north', 'east', 'south', 'west', 'north']
    this.#bearing = dirs[dirs.indexOf(this.#bearing) + 1] as Direction
  }

  turnLeft: () => void = () => {
    const dirs = ['north', 'east', 'south', 'west', 'north']
    this.#bearing = dirs[dirs.lastIndexOf(this.#bearing) - 1] as Direction
  }

  advance: () => void = () => {
    const v = ['north', 'south']
    const h = ['east', 'west']
    const [x, y] = this.#coordinates
    if (v.includes(this.#bearing)) {
      this.#coordinates[1] = y + (this.#bearing === 'north' ? 1 : -1)
    } else {
      this.#coordinates[0] = x + (this.#bearing === 'east' ? 1 : -1)
    }
  }
}
