const response = {
  whatever: 'Whatever.',
  chill: 'Whoa, chill out!',
  sure: 'Sure.',
  calm: 'Calm down, I know what I\'m doing!',
  fine: 'Fine. Be that way!'
}

const question = (message:string):boolean =>
  message.endsWith('?')

const shouting = (message:string):boolean => {
  const cleaned:string = message.replace(/[^a-z]/gi, '')
  return ((cleaned > '') && (cleaned.toUpperCase() === cleaned))
}

export function hey(message: string): string {
  message = message.trim()
  if (message === '') return response.fine
  if (question(message) && shouting(message)) return response.calm
  if (question(message)) return response.sure
  if (shouting(message)) return response.chill
  return response.whatever
}
