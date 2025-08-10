const allDays: string[] = 'first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth'.split(' ');
const allThings: string[] = [
  'a Partridge in a Pear Tree.',
  'two Turtle Doves',
  'three French Hens',
  'four Calling Birds',
  'five Gold Rings',
  'six Geese-a-Laying',
  'seven Swans-a-Swimming',
  'eight Maids-a-Milking',
  'nine Ladies Dancing',
  'ten Lords-a-Leaping',
  'eleven Pipers Piping',
  'twelve Drummers Drumming',
]

const formatList = (l: string[]): string => {
  const beginning = l.slice(0, -1).join(', ');
  const end = l.slice(-1)[0];
  return (beginning.length === 0) ? end : `${beginning}, and ${end}`;
}

export function recite(from: number, to: number): string {
  const days = allDays.slice(from - 1, to);
  const str = days.map((day: string, i: number): string => {
    const things = allThings.slice(0, from + i);
    const dayThings = formatList(things.reverse());
    return `On the ${day} day of Christmas my true love gave to me: ${dayThings}`;
  }).join("\n");
  return str + "\n";
}
