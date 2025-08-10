const animals = [
  {kind: "fly", verse: `I don't know why she swallowed the fly. Perhaps she'll die.`},
  {kind: "spider", verse: "It wriggled and jiggled and tickled inside her."},
  {kind: "bird", verse: "How absurd to swallow a bird!"},
  {kind: "cat", verse: "Imagine that, to swallow a cat!"},
  {kind: "dog", verse: "What a hog, to swallow a dog!"},
  {kind: "goat", verse: "Just opened her throat and swallowed a goat!"},
  {kind: "cow", verse: "I don't know how she swallowed a cow!"},
  {kind: "horse", verse: "She's dead, of course!"}
]

function start(animal_name) {
  return `I know an old lady who swallowed a ${animal_name}.`
}

function combine(curr_animal, prev_animal) {
  return `She swallowed the ${curr_animal} to catch the ${prev_animal}.`
}

function do_spider([first, second]) {
  return first.replace('.', ' ') + second.replace('It ', 'that ');
}

function add_verse(verseIdx) {
  // verseIdx is 0 based
  const curr_animal = animals[verseIdx].kind;
  const prev_animal = animals[verseIdx - 1].kind;
  let phrases =
    [combine(curr_animal, prev_animal), animals[verseIdx - 1].verse];
  if ( verseIdx > 2) {
    phrases = [phrases[0]];
  } else if ( verseIdx === 2) {
    phrases = [do_spider([phrases[0], phrases[1]])];
  }
  return phrases;
}

export class Song {
  verse(verseNum) {
    // verseNum is 1 based switch to 0 base and go down from there
    const maxVerse = verseNum - 1;
    const kind = animals[maxVerse].kind;
    const v = animals[maxVerse].verse;
    let phrases = [start(kind), v];
    if ( verseNum === 8 ) return phrases.join("\n") + "\n";
    // when maxVerse === 0 the for loop is skipped
    for ( let i = maxVerse; i > 0; i-- ) {
      phrases = phrases.concat(add_verse(i));
    }
    return phrases.join("\n") + "\n";
  }

  verses(startNum, stopNum) {
    return [...Array(stopNum - startNum + 1).keys()].map((num) => this.verse(num + 1)).join("\n") + "\n";
  }
}
