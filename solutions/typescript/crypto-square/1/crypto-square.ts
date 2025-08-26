export class Crypto {
  private normalizedText: string

  constructor(plainText: string) {
    this.normalizedText = plainText
      .toLowerCase()
      .replace(/[^a-z0-9]/g, '')
  }

  get ciphertext(): string {
    if (!this.normalizedText) return ''

    const length = this.normalizedText.length
    const cols = Math.ceil(Math.sqrt(length))
    const rows = Math.ceil(length / cols)

    const chunks: string[] = []
    for (let col = 0; col < cols; col++) {
      let chunk = ''
      for (let row = 0; row < rows; row++) {
        const index = row * cols + col
        chunk += index < length ? this.normalizedText[index] : ' '
      }
      chunks.push(chunk)
    }

    return chunks.join(' ')
  }
}
