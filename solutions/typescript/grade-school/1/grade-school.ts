interface Roster {
  [grade: number]: string[]
}

export class GradeSchool {
  #roster: Roster = {} as Roster

  roster(): Roster {
    let out: Roster = Object.assign({}, this.#roster);
    Object.entries(out).forEach(([grade, students]) => {
      const g = parseInt(grade, 10);
      out[g] = [...students]
    })
    return out;
  }

  add(student: string, grade: number): void {
    this.remove(student);
    if ( typeof this.#roster[grade] !== 'undefined' ) {
      this.#roster[grade].push(student);
      this.#roster[grade].sort();
    } else {
      this.#roster[grade] = [student];
    }
  }

  grade(grade: number): string[] {
    if ( typeof this.#roster[grade] === 'undefined' ) return []
    return [...this.#roster[grade]];
  }

  private remove(student: string): void {
    for ( let grade in this.#roster ) {
      const i: number = this.#roster[grade].indexOf(student);
      if ( i >= 0 ) this.#roster[grade].splice(i, 1);
    }
  }
}
