# Score categories
YACHT = lambda d: 50 if len(set(d)) == 1 else 0
ONES = lambda d: sum(i for i in d if i == 1)
TWOS = lambda d: sum(i for i in d if i == 2)
THREES = lambda d: sum(i for i in d if i == 3)
FOURS = lambda d: sum(i for i in d if i == 4)
FIVES = lambda d: sum(i for i in d if i == 5)
SIXES = lambda d: sum(i for i in d if i == 6)
FULL_HOUSE = lambda d: sum(d) if len(set(d)) == 2 and any(d.count(i) == 3 for i in set(d)) else 0
FOUR_OF_A_KIND = lambda d: sum(i * 4 for i in set(d) if d.count(i) > 3)
LITTLE_STRAIGHT = lambda d: 30 if sum(d) == 15 and len(set(d)) == 5 else 0
BIG_STRAIGHT = lambda d: 30 if sum(d) == 20 and len(set(d)) == 5 else 0
CHOICE = lambda d: sum(d)

def score(dice, category):
    if any(not 0 < x < 7 for x in dice):
        raise ValueError("Invalid dice {}".format(dice))

    return category(dice)