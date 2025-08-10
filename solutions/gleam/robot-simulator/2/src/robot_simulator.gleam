import gleam/string
import gleam/list

pub type Robot {
  Robot(direction: Direction, position: Position)
}

pub type Direction {
  North
  East
  South
  West
}

pub type Position {
  Position(x: Int, y: Int)
}

pub fn create(direction: Direction, position: Position) -> Robot {
  Robot(direction, position)
}

pub fn move(
  direction: Direction,
  position: Position,
  instructions: String,
) -> Robot {
  instructions
  |> string.to_graphemes()
  |> list.fold(create(direction, position), move_one)
}

fn move_one(robot: Robot, step: String) -> Robot {
  case step {
    "R" -> turn_right(robot)
    "L" -> turn_left(robot)
    "A" -> advance(robot)
  }
}

fn turn_right(robot: Robot) -> Robot {
  let dir = case robot.direction {
    North -> East
    East -> South
    South -> West
    West -> North
  }
  Robot(..robot, direction: dir)
}

fn turn_left(robot: Robot) -> Robot {
  let dir = case robot.direction {
    North -> West
    East -> North
    South -> East
    West -> South
  }
  Robot(..robot, direction: dir)
}

fn advance(robot: Robot) -> Robot {
  let pos = robot.position
  let #(x, y) = case robot.direction {
    North -> #(pos.x, pos.y + 1)
    East -> #(pos.x + 1, pos.y)
    South -> #(pos.x, pos.y - 1)
    West -> #(pos.x - 1, pos.y)
  }
  Robot(..robot, position: Position(x, y))
}
