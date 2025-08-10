# Globals for the directions
EAST = 'east'
NORTH = 'north'
WEST = 'west'
SOUTH = 'south'


class Robot:
    def __init__(self, direction=NORTH, x_pos=0, y_pos=0):
        self.direction = direction
        self.coordinates = (x_pos, y_pos)

    def move(self, steps):
        for step in steps:
            if step == 'R':
                self.turn_right()
            elif step == 'L':
                self.turn_left()
            elif step == 'A':
                self.advance()

    def turn_right(self):
        self.direction = {
            NORTH: EAST,
            EAST: SOUTH,
            SOUTH: WEST,
            WEST: NORTH
        }[self.direction]

    def turn_left(self):
        self.direction = {
            NORTH: WEST,
            WEST: SOUTH,
            SOUTH: EAST,
            EAST: NORTH
        }[self.direction]

    def advance(self):
        x, y = self.coordinates
        if self.direction == NORTH:
            self.coordinates = (x, y + 1)
        elif self.direction == SOUTH:
            self.coordinates = (x, y - 1)
        elif self.direction == EAST:
            self.coordinates = (x + 1, y)
        elif self.direction == WEST:
            self.coordinates = (x - 1, y)
