export class DiffieHellman {
  p: number;
  g: number;

  constructor(p: number, g: number) {
    if(p > 100 || g > 100 || !isPrime(p) || !isPrime(g)){
      throw new Error()
    }
    this.p = p;
    this.g = g;
  }

  public getPublicKey(privateKey: number): number {
    if ( privateKey > 1 && privateKey < this.p) { 
      return this.g ** privateKey % this.p;
    } else {
      throw new Error(`Beep Boop, Private key not valid`);
    }
  }

  public getSecret(theirPublicKey: number, myPrivateKey: number): number {
    return theirPublicKey ** myPrivateKey % this.p 
  }
}

function isPrime(candidate: number): boolean {
  for(let i = 2, s = Math.sqrt(candidate); i <= s; i++) {
    if(candidate % i === 0) return false;
  }
  return candidate > 1;
}