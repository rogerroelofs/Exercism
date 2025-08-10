from itertools import groupby

def decode(encoded):
    count = ''
    s = ''
    for k, g in groupby(encoded):
        if k.isnumeric():
            count += str(k)
        elif not count:
            s += str(k)
        else:
            s += str(int(count) * k)
            count = ''
    return s

def encode(input):
    encoded = ''
    for k, g in groupby(input):
        section = sum(1 for _ in g)
        if section > 1:
            encoded += str(section) + k
        else:
             encoded += k
    return encoded