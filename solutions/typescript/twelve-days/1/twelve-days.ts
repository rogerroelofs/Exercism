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

const formatter = new Intl.ListFormat('en', { style: 'long', type: 'conjunction' });

const formatList = (l: string[]): string => {
  const thingsCount = l.length;
  let formatted = formatter.format(l.reverse());
  if (thingsCount) {
    // Intl doesn't add a comma before and in a 2 item list so we add one to match the spec.
    formatted = formatted.replace('Doves and', 'Doves, and')
  }
  return formatted;
}

export function recite(from: number, to: number): string {
  const days = allDays.slice(from - 1, to);
  const str = days.map((day: string, i: number): string => {
    const things = allThings.slice(0, from + i);
    const dayThings = formatList(things);
    return `On the ${day} day of Christmas my true love gave to me: ${dayThings}`;
  }).join("\n");
  return str + "\n";
}
