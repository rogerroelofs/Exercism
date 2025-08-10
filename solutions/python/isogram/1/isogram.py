def is_isogram(input_string: str) -> bool:
    if not input_string:
        return True

    cleaned = ''.join(char.lower() for char in input_string if char.isalnum())
    return len(cleaned) == len(set(cleaned))