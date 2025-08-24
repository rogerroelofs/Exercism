type WordData = {
  [word: string]: { start: [number, number]; end: [number, number] } | undefined;
};

export class WordSearch {
  private readonly block: string[];
  private readonly rotatedBlock: string[];

  constructor(block: string[]) {
    this.block = block;
    this.rotatedBlock = this.rotateBlock(block);
  }

  public find(words: string[]) {
    const foundWords: WordData = {};
    for (const word of words) {
      const word_data = this.searchWord(word);
      if (word_data[word]) {
        foundWords[word] = word_data[word];
      }
    }
    return foundWords;
  }

  private searchWord(word: string): WordData {
    for (let i = 0; i < this.block.length; i++) {
      const line = this.block[i];
      let found: WordData | null = null;
      for (let j = 0; j < line.length; j++) {
        found = this.checkHorizontal(word, i, j);
        if (found) return found;
        found = this.checkVertical(word, i, j);
        if (found) return found;
        found = this.checkDiagonal(word, i, j);
        if (found) return found;
      }
    }
    return { [word]: undefined }
  }

  private rotateBlock(block: string[]): string[] {
    const rotated: string[] = [];
    for (let j = 0; j < block[0].length; j++) {
      let newRow = '';
      for (let i = 0; i < block.length; i++) {
        newRow += block[i][j];
      }
      rotated.push(newRow);
    }
    return rotated;
  }

  private checkHorizontal(word: string, i: number, j: number, block = this.block): WordData | null {
    const line = block[i];
    const reverseWord = word.split('').reverse().join('');
    const startIndex = line.indexOf(word);
    const reverseStartIndex = line.indexOf(reverseWord);
    if (startIndex !== -1) {
      return {
        [word]: {
          start: [i + 1, startIndex + 1],
          end: [i + 1, startIndex + word.length],
        },
      };
    }
    if (reverseStartIndex !== -1) {
      return {
        [word]: {
          start: [i + 1, reverseStartIndex + word.length],
          end: [i + 1, reverseStartIndex + 1],
        },
      };
    }
    return null;
  }

  private checkVertical(word: string, i: number, j: number): WordData | null {
    let found = this.checkHorizontal(word, j, i, this.rotatedBlock);
    // transpose
    if (found) {
      const start = found[word]?.start;
      const end = found[word]?.end;
      if (start && end) {
        return {
          [word]: {
            start: [start[1], start[0]],
            end: [end[1], end[0]],
          },
        };
      }
    }
    return null;
  }

  private checkDiagonal(word: string, i: number, j: number): WordData | null {
    const directions = [
      { name: 'ul', dx: -1, dy: -1 },
      { name: 'ur', dx: -1, dy: 1 },
      { name: 'll', dx: 1, dy: -1 },
      { name: 'lr', dx: 1, dy: 1 },
    ];
    for (const { dx, dy } of directions) {
      let chars = '';
      for (let k = 0; k < word.length; k++) {
        const x = i + dx * k;
        const y = j + dy * k;
        if (x < 0 || x >= this.block.length || y < 0 || y >= this.block[0].length) break;
        chars += this.block[x][y];
      }
      if (chars === word) {
        return {
          [word]: {
            start: [i + 1, j + 1],
            end: [i + dx * (word.length - 1) + 1, j + dy * (word.length - 1) + 1],
          },
        };
      }
      if (chars === word.split('').reverse().join('')) {
        return {
          [word]: {
            start: [i + dx * (word.length - 1) + 1, j + dy * (word.length - 1) + 1],
            end: [i + 1, j + 1],
          },
        };
      }
    }
    return null;
  }

}
