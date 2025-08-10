export function makeDiamond(character: string): string {
  const buildShape = (letter: string): string => {
    const width = (letter.charCodeAt(0) - 'A'.charCodeAt(0)) * 2 + 1;

    const buildLine = (letter: string, width: number): string => {
      const innerWidth = (letter.charCodeAt(0) - 'A'.charCodeAt(0)) * 2 - 1;
      const outerWidth = Math.floor((width - ((letter.charCodeAt(0) - 'A'.charCodeAt(0)) * 2 + 1)) / 2);
      const letterStr = letter;
      const outerPad = ' '.repeat(outerWidth);

      const content = innerWidth < 1
        ? letterStr
        : letterStr + ' '.repeat(innerWidth) + letterStr;

      return outerPad + content + outerPad;
    };

    const front = Array.from({ length: letter.charCodeAt(0) - 'A'.charCodeAt(0) + 1 }, (_, i) => 
      buildLine(String.fromCharCode('A'.charCodeAt(0) + i), width)
    );

    return front.concat(front.slice(0, -1).reverse()).join('\n') + '\n';
  };

  return buildShape(character);
}