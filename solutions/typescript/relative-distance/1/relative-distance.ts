export function degreesOfSeparation(
  familyTree: Record<string, string[]>,
  personA: string,
  personB: string
): number {
  if (personA === personB) return 0;

  // Helper to build child -> parent(s) map
  function buildChildToParents(tree: Record<string, string[]>): Record<string, string[]> {
    const map: Record<string, string[]> = {};
    for (const parent in tree) {
      for (const child of tree[parent]) {
        if (!map[child]) map[child] = [];
        map[child].push(parent);
      }
    }
    return map;
  }

  // Helper to get relatives (children, parents, siblings)
  function getRelatives(person: string): string[] {
    const relatives = new Set<string>();
    // Children
    for (const child of familyTree[person] || []) {
      relatives.add(child);
    }
    // Parents
    for (const parent of childToParents[person] || []) {
      relatives.add(parent);
      // Siblings
      for (const sibling of familyTree[parent] || []) {
        if (sibling !== person) relatives.add(sibling);
      }
    }
    return Array.from(relatives);
  }

  const childToParents = buildChildToParents(familyTree);
  const visited = new Set<string>();
  const queue: [string, number][] = [[personA, 0]];
  while (queue.length > 0) {
    const [current, degree] = queue.shift()!;
    if (current === personB) {
      return degree;
    }
    if (!visited.has(current)) {
      visited.add(current);
      for (const relative of getRelatives(current)) {
        if (!visited.has(relative)) {
          queue.push([relative, degree + 1]);
        }
      }
    }
  }
  return -1;
}
