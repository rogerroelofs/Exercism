def translate(text):
    words = text.split()
    pig_latin = []
    for word in words:
        if word[0:2] == 'yt' or word[0:2] == 'xr':
            pig_latin.append(word + 'ay')
        elif word[0:3] == 'sch' or word[0:3] == 'thr' or word[0:3] == 'squ':
            pig_latin.append(word[3:] + word[0:3] + 'ay')
        elif word[0:2] == 'ch' or word[0:2] == 'qu' or word[0:2] == 'th' or word[0:2] == 'rh':
            pig_latin.append(word[2:] + word[0:2] + 'ay')
        elif word[0] in 'aeiou':
            pig_latin.append(word + 'ay')
        else:
            pig_latin.append(word[1:] + word[0] + 'ay')
    return ' '.join(pig_latin)
