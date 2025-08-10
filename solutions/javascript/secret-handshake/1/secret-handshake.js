const shakes = ['wink', 'double blink', 'close your eyes', 'jump'];

export const commands = (input) => {
  const bin = input.toString(2).split("").reverse();
  let handshake = []
  for (let i = 0; i < 4; i++){
    if (bin[i] === "1") handshake.push(shakes[i])
  }

  return bin[4] === "1" ? handshake.reverse() : handshake;
};
