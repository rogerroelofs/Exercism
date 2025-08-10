defmodule Bowling do
  @doc """
    Creates a new game of bowling that can be used to store the results of
    the game
  """

  @spec start() :: any
  def start do
    %{complete: false, curr_frame: 0, frames: List.duplicate([], 10)}
  end

  @doc """
    Records the number of pins knocked down on a single roll. Returns `any`
    unless there is something wrong with the given number of pins, in which
    case it returns a helpful error tuple.
  """

  @spec roll(any, integer) :: {:ok, any} | {:error, String.t()}
  def roll(%{complete: complete}, _) when complete, do: {:error, "Cannot roll after game is over"}
  def roll(_, roll) when roll < 0, do: {:error, "Negative roll is invalid"}
  def roll(_, roll) when roll > 10, do: {:error, "Pin count exceeds pins on the lane"}

  def roll(game, roll) when game.curr_frame == 9 do
    # allow for up to 2 extra balls
    frame = case Enum.at(game.frames, game.curr_frame) do
      [] -> [roll]
      [first] -> [first, roll]
      [first, second] when first + second >= 10 ->
        [first, second, roll]
    end

    if (! validate_last_frame(frame)) do
      {:error, "Pin count exceeds pins on the lane"}
    else
      {:ok, update_game_state(game, frame)}
    end
  end

  def roll(game, roll) do
    frame = case Enum.at(game.frames, game.curr_frame) do
      [] -> [roll]
      [first] -> [first, roll]
    end
    if Enum.sum(frame) > 10 do
      {:error, "Pin count exceeds pins on the lane"}
    else
      {:ok, update_game_state(game, frame)}
    end
  end

  @doc """
    Returns the score of a given game of bowling if the game is complete.
    If the game isn't complete, it returns a helpful error tuple.
  """

  @spec score(any) :: {:ok, integer} | {:error, String.t()}
  def score(game) do
    if game.complete do
      score =
        game.frames
        |> Enum.with_index()
        |> Enum.reduce(0, fn {frame, i}, acc ->
          case frame do
            [10] ->
              acc + 10 + score_strike_bonus(game.frames, i + 1)
            [first, second] when first + second == 10 ->
              acc + first + second + Enum.at(Enum.at(game.frames, i + 1), 0)
            [first, second] -> acc + first + second
            [first, second, third] -> acc + first + second + third
          end
        end)
      {:ok, score}
    else
      {:error, "Score cannot be taken until the end of the game"}
    end
  end

  defp score_strike_bonus(frames, i) do
    bonus1 = Enum.at(Enum.at(frames, i), 0)
    bonus2 = if bonus1 == 10 and i < 9 do
      Enum.at(Enum.at(frames, i + 1), 0)
    else
      Enum.at(Enum.at(frames, i), 1)
    end
    bonus1 + bonus2
  end

  defp update_game_state(game, frame) do
    frames = List.update_at(game.frames, game.curr_frame, fn _ -> frame end)
    curr_frame = case frame do
      [10] when game.curr_frame == 9 -> game.curr_frame
      [10] -> game.curr_frame + 1
      [roll] when roll < 10 -> game.curr_frame
      [r1, r2] when game.curr_frame == 9 and r1 + r2 >= 10 -> game.curr_frame
      [_, _] -> game.curr_frame + 1
      [_, _, _] -> game.curr_frame + 1
    end
    complete = case curr_frame do
      10 -> true
      _ -> false
    end
    %{game | complete: complete, curr_frame: curr_frame, frames: frames}
  end

  defp validate_last_frame(frame) do
    case frame do
      [_] -> true
      [10, _] -> true
      [10, 10, _] -> true
      [10, first, second] when first + second <= 10 -> true
      [10, first, second] when first + second > 10 -> false
      [first, second, third] when first + second == 10 and third <= 10 -> true
      [first, second] -> (first + second) <= 10
    end
  end
end
