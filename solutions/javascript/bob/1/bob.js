const response = {
  whatever: 'Whatever.',
  chill: 'Whoa, chill out!',
  sure: 'Sure.',
  calm: 'Calm down, I know what I\'m doing!',
  fine: 'Fine. Be that way!'
}

const question = (message) => message.endsWith('?')

const shouting = (message) => {
  const cleaned = message.replace(/[^a-z]/gi, '')
  return ((cleaned > '') && (cleaned.toUpperCase() === cleaned))
}

export const hey = (message) => {
  message = message.trim()
  if (message === '') return response.fine
  if (question(message) && shouting(message)) return response.calm
  if (question(message)) return response.sure
  if (shouting(message)) return response.chill
  return response.whatever
}
