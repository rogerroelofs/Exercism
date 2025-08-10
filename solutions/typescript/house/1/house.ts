const VERSES: string[] = `the house that Jack built.
the malt\tthat lay in
the rat\tthat ate
the cat\tthat killed
the dog\tthat worried
the cow with the crumpled horn\tthat tossed
the maiden all forlorn\tthat milked
the man all tattered and torn\tthat kissed
the priest all shaven and shorn\tthat married
the rooster that crowed in the morn\tthat woke
the farmer sowing his corn\tthat kept
the horse and the hound and the horn\tthat belonged to`.split("\n");


export function verse(maxVerse: number): string[] {
  let ret: string[] = VERSES.slice(0, maxVerse)
    .reverse()
    .join(" ")
    .replaceAll("\t", "\n").split("\n");
  ret[0] = `This is ${ret[0]}`;
  return ret;
}

export function verses(startNum: number, stopNum: number): string[] {
  return [...Array(stopNum - startNum + 1).keys()]
    .flatMap((num: number): string[] => [...verse(num + startNum), ''])
    .slice(0, -1);
}
