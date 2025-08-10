
const DEFAULT_STUDENTS = [
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
];

const PLANT_CODES = {
  G: 'grass',
  V: 'violets',
  R: 'radishes',
  C: 'clover',
};

export class Garden {
  constructor(privatediagram, students = DEFAULT_STUDENTS) {
    this.students = students.sort();
    this.diagram = privatediagram.split('\n').map((row) => row.split(''));
  }

  plants(student) {
    const studentIndex = this.students.indexOf(student)
    const studentPots = this.diagram.map(row => row.slice(studentIndex * 2, studentIndex * 2 + 2)).flat()
    return studentPots.map(plantCode => PLANT_CODES[plantCode])
  }
}
