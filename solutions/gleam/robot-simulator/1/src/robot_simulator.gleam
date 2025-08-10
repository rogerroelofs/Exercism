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
    "R" ->
      robot
      |> turn_right
    "L" ->
      robot
      |> turn_left
    "A" ->
      robot
      |> advance
  }
}

fn turn_right(robot: Robot) -> Robot {
  case robot.direction {
    North -> Robot(..robot, direction: East)
    East -> Robot(..robot, direction: South)
    South -> Robot(..robot, direction: West)
    West -> Robot(..robot, direction: North)
  }
}

fn turn_left(robot: Robot) -> Robot {
  case robot.direction {
    North -> Robot(..robot, direction: West)
    East -> Robot(..robot, direction: North)
    South -> Robot(..robot, direction: East)
    West -> Robot(..robot, direction: South)
  }
}

fn advance(robot: Robot) -> Robot {
  case robot.direction {
    North ->
      Robot(..robot, position: Position(robot.position.x, robot.position.y + 1))
    East ->
      Robot(..robot, position: Position(robot.position.x + 1, robot.position.y))
    South ->
      Robot(..robot, position: Position(robot.position.x, robot.position.y - 1))
    West ->
      Robot(..robot, position: Position(robot.position.x - 1, robot.position.y))
  }
}
