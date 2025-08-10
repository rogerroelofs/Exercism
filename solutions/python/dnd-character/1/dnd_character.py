from random import randint

ATTRS = (
    'strength', 'dexterity', 'constitution',
    'intelligence', 'wisdom', 'charisma')

def modifier(score):
    return (score - 10) // 2

class Character:
    def __init__(self):
        for attr in ATTRS:
            setattr(self, attr, self.ability())
        self.hitpoints = 10 + modifier(self.constitution)

    def ability(self):
        return randint(3, 18)
