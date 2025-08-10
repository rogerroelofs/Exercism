RESPONSE = {
    'whatever': 'Whatever.',
    'chill': 'Whoa, chill out!',
    'sure': 'Sure.',
    'calm': "Calm down, I know what I'm doing!",
    'fine': 'Fine. Be that way!'
}

def is_question(message: str) -> bool:
    return message.strip().endswith('?')

def is_shouting(message: str) -> bool:
    cleaned = ''.join(char for char in message if char.isalpha())
    return cleaned.isupper() and cleaned

def response(message: str) -> str:
    message = message.strip()
    if not message:
        return RESPONSE['fine']
    if is_question(message) and is_shouting(message):
        return RESPONSE['calm']
    if is_question(message):
        return RESPONSE['sure']
    if is_shouting(message):
        return RESPONSE['chill']
    return RESPONSE['whatever']