ANIMALS = {
    1:('fly','I don\'t know why she swallowed the fly. Perhaps she\'ll die.', ''),
    2:('spider','It wriggled and jiggled and tickled inside her.', ' that wriggled and jiggled and tickled inside her'),
    3:('bird','How absurd to swallow a bird!', ''),
    4:('cat','Imagine that, to swallow a cat!', ''),
    5:('dog','What a hog, to swallow a dog!', ''),
    6:('goat','Just opened her throat and swallowed a goat!', ''),
    7:('cow','I don\'t know how she swallowed a cow!', ''),
    8:('horse','She\'s dead, of course!', '')
    
}

START = 'I know an old lady who swallowed a {}.'
COMBINED = 'She swallowed the {} to catch the {}{}.'

def get_verse(verse):
    verse_text = [START.format(ANIMALS[verse][0]), ANIMALS[verse][1]]

    if verse < 8:
        for i in reversed(range(2, verse + 1)):
            verse_text.append(COMBINED.format(ANIMALS[i][0], ANIMALS[i - 1][0], ANIMALS[i - 1][2]))
        if verse != 1:
            verse_text.append(ANIMALS[1][1])

    return verse_text

def recite(start, end):
    return_list = []

    for i in range(start, end + 1):
        if i != start:
            return_list.append('')
        return_list.extend(get_verse(i))

    return return_list