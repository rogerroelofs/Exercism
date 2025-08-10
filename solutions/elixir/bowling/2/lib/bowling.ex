defmodule Bowling do
  require Bowling
  @pin_max 10
  @pin_min 0
  @max_frames 10
  defguard final_frame(frames) when length(frames) == @max_frames - 1
  defguard is_spare(r1, r2) when r1 + r2 == @pin_max

@doc """
    Creates a new game of bowling that can be used to store the results of
    the game
  """

  @spec start() :: any
  def start, do: []

  @doc """
    Records the number of pins knocked down on a single roll. Returns `any`
    unless there is something wrong with the given number of pins, in which
    case it returns a helpful error tuple.
  """

  @spec roll(any, integer) :: {:ok, any} | {:error, String.t()}
  def roll(_, roll) when roll < @pin_min, do: {:error, "Negative roll is invalid"}
  def roll(_, roll) when roll > @pin_max, do: {:error, "Pin count exceeds pins on the lane"}
  # illegal balls final frame
  def roll([[r1, r2] | rest], _roll) when final_frame(rest) and r1 + r2 < @pin_max do
    {:error, "Cannot roll after game is over"}
  end
  def roll([[_r1, _r2, _r3] | rest], _roll) when final_frame(rest) do
    {:error, "Cannot roll after game is over"}
  end

  # last frame special cases
  # spare in last frame
  def roll([[r1, r2] | rest], roll) when final_frame(rest) and is_spare(r1, r2) do
    {:ok, [[r1, r2, roll] | rest]}
  end
  # strikes in last frame
  def roll([[@pin_max] | rest], roll) when final_frame(rest) do
    {:ok, [[@pin_max, roll] | rest]}
  end
  def roll([[@pin_max, @pin_max] | rest], roll) when final_frame(rest) do
    {:ok, [[@pin_max, @pin_max, roll] | rest]}
  end
  def roll([[@pin_max, r2] | rest], roll) when final_frame(rest) and r2 + roll <= @pin_max do
    {:ok, [[@pin_max, r2, roll] | rest]}
  end
  # too many pins
  def roll([[@pin_max, r2] | rest], roll) when final_frame(rest) and r2 + roll > @pin_max do
    {:error, "Pin count exceeds pins on the lane"}
  end

  # if prev frame was a strike start a new frame
  def roll([[@pin_max] | _rest] = frames, roll), do: {:ok, [[roll] | frames]}

  # second roll of each frame
  def roll([[r1] | _rest], roll) when r1 + roll > @pin_max, do: {:error, "Pin count exceeds pins on the lane"}
  def roll([[r1] | rest], roll), do: {:ok, [[r1, roll] | rest]}

  # first roll of each frame - this has to be last
  def roll(frames, roll), do: {:ok, [[roll] | frames]}

  @doc """
    Returns the score of a given game of bowling if the game is complete.
    If the game isn't complete, it returns a helpful error tuple.
  """

  @spec score(any) :: {:ok, integer} | {:error, String.t()}
  def score(frames) when length(frames) < @max_frames do
    {:error, "Score cannot be taken until the end of the game"}
  end
  def score([[@pin_max] | rest]) when final_frame(rest) do
    {:error, "Score cannot be taken until the end of the game"}
  end
  def score([[@pin_max, @pin_max] | rest]) when final_frame(rest) do
    {:error, "Score cannot be taken until the end of the game"}
  end
  def score([[r1, r2] | rest]) when final_frame(rest) and is_spare(r1, r2) do
    {:error, "Score cannot be taken until the end of the game"}
  end
  def score(frames) do
    score =
      frames
      |> Enum.with_index()
      |> Enum.reduce(0, fn {frame, i}, acc ->
        case frame do
          [@pin_max] ->
            acc + @pin_max + score_strike_bonus(frames, i - 1)
          [first, second] when is_spare(first, second) ->
            acc + first + second + Enum.at(Enum.at(frames, i - 1), 0)
          [first, second] -> acc + first + second
          [first, second, third] ->
            acc + first + second + third
        end
      end)
    {:ok, score}
  end

  defp score_strike_bonus(frames, i) do
    case Enum.at(frames, i) do
      [@pin_max] -> @pin_max + Enum.at(Enum.at(frames, i - 1), 0)
      [r1, r2] -> r1 + r2
      [@pin_max, r1, _r2] -> @pin_max + r1
    end
  end

end
