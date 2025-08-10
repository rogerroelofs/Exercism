//
// This is only a SKELETON file for the 'Kindergarten Garden' exercise.
// It's been provided as a convenience to get you started writing code faster.
//

const DEFAULT_STUDENTS: Student[] = [
  'Alice',
  'Bob',
  'Charlie',
  'David',
  'Eve',
  'Fred',
  'Ginny',
  'Harriet',
  'Ileana',
  'Joseph',
  'Kincaid',
  'Larry',
]

const PLANT_CODES = {
  G: 'grass',
  V: 'violets',
  R: 'radishes',
  C: 'clover',
} as const

type Student = string
type Plant = (typeof PLANT_CODES)[keyof typeof PLANT_CODES]
type Plants = Plant[]
type Pots = Plants[]

export class Garden {
  pots: string[][];
  
  constructor(diagram: string, private students = DEFAULT_STUDENTS) {
    this.pots = diagram.split('\n').map(row => row.split(''))
    this.students = students.sort()
  }

  public plants(student: Student): Plants {
    const studentIndex = this.students.indexOf(student)
    const studentPots = this.pots.map(row => row.slice(studentIndex * 2, studentIndex * 2 + 2)).flat()
    return studentPots.map(plantCode => PLANT_CODES[plantCode as keyof typeof PLANT_CODES])
  }
}
