defmodule RobotSimulator do
  @type robot() :: any()
  @type direction() :: :north | :east | :south | :west
  @type position() :: {integer(), integer()}

  defmodule Robot do
    defstruct direction: :north, position: {0, 0}
  end

  @doc """
  Checks whether direction is valid

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  defguardp is_direction(direction) when direction in [:north, :east, :south, :west]
  @doc """
  Checks whether position is valid

  Valid position is two integers
  """
  defguardp is_position(x, y) when is_integer(x) and is_integer(y)

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction, position) :: robot() | {:error, String.t()}
  def create(), do: %Robot{}
  def create(direction, _) when not is_direction(direction), do: {:error, "invalid direction"}
  def create(direction, {x, y}) when is_direction(direction) and is_position(x, y), do: %Robot{direction: direction, position: {x, y}}
  def create(_, _), do: {:error, "invalid position"}

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot, instructions :: String.t()) :: robot() | {:error, String.t()}
  def simulate(robot, ""), do: robot
  def simulate(%Robot{direction: direction, position: position} = robot, instructions) do
    {first, rest} = String.split_at(instructions, 1)
    case first do
      "R" ->
        %Robot{robot | direction: right(direction)} |> simulate(rest)
      "L" ->
        %Robot{robot | direction: left(direction)} |> simulate(rest)
      "A" ->
        %Robot{robot | position: advance(direction, position)} |> simulate(rest)
      _ ->
        {:error, "invalid instruction"}
    end
  end

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot) :: direction()
  def direction(%Robot{direction: direction}), do: direction

  @doc """
  Return the robot's position.
  """
  @spec position(robot) :: position()
  def position(%Robot{position: position}), do: position

  defp advance(direction, {x, y}) do
    case direction do
      :north -> {x, y + 1}
      :east -> {x + 1, y}
      :south -> {x, y - 1}
      :west -> {x - 1, y}
    end
  end

  defp right(direction) do
    case direction do
      :north -> :east
      :east -> :south
      :south -> :west
      :west -> :north
    end
  end

  defp left(direction) do
    case direction do
      :north -> :west
      :east -> :north
      :south -> :east
      :west -> :south
    end
  end
end
