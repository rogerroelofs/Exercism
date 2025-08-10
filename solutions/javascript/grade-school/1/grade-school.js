export class GradeSchool {
  #roster = {};
  roster() {
    return structuredClone(this.#roster);
  }

  add(student, grade) {
    this.#maybeSetupGrade(grade);
    this.#maybeRemoveStudent(student);
    this.#roster[grade].push(student);
    this.#roster[grade].sort();  
  }

  grade(grade) {
    this.#maybeSetupGrade(grade);
    return structuredClone(this.#roster[grade]);
  }

  // helper methods
  // setup grade if not exists
  #maybeSetupGrade(grade) {
    if (!this.#roster[grade]) {
      this.#roster[grade] = [];
    }
  }

  // student names are unique, remove if exists before adding
  #maybeRemoveStudent(student) {
    for (const grade in this.#roster) {
      this.#roster[grade] = this.#roster[grade].filter(s => s !== student);
    }
  }
}
