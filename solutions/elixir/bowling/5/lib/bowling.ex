defmodule Bowling do
  @type pins :: 0..10
  @type frame :: 1..10
  @type valid_points :: 0..300
  @type bonus_rolls :: 0..2

  @pin_max 10
  @pin_min 0
  @max_frames 10
  @penultimate_frame @max_frames - 1

  defguardp is_spare(r1, r2) when r1 + r2 == @pin_max

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

  @spec roll([pins], Integer.t()) :: {:ok, [pins]} | {:error, String.t()}
  def roll(rolls, roll) do
    if roll < 0 do
      {:error, "Negative roll is invalid"}
    else
      case game_state(rolls) do
        {:game_over, _} ->
          {:error, "Cannot roll after game is over"}

        {:active, pins_on_lane, _, _} ->
          if roll > pins_on_lane do
            {:error, "Pin count exceeds pins on the lane"}
          else
            {:ok, rolls ++ [roll]}
          end
      end
    end
  end

  @doc """
    Returns the score of a given game of bowling if the game is complete.
    If the game isn't complete, it returns a helpful error tuple.
  """

  @spec score([pins]) :: {:ok, integer} | {:error, String.t()}
  def score(rolls) do
    case game_state(rolls) do
      {:game_over, {score, _}} ->
        {:ok, score}

      {:active, _, _, _} ->
        {:error, "Score cannot be taken until the end of the game"}
    end
  end

  @spec game_state([pins], frame, {valid_points, bonus_rolls}) ::
          {:active, pins, frame, {valid_points, bonus_rolls}} | {:game_over, {valid_points, 0}}
  defp game_state(rolls, frame \\ 0, score_and_bonus \\ {0, 0})

  # last frame: check bonus rolls
  # spares
  defp game_state([r1, r2, b1], @penultimate_frame, score_and_bonus) when is_spare(r1, r2),
    do: {:game_over, score_and_bonus |> add_score(r1, r2) |> add_bonus_score(b1)}

  defp game_state([r1, r2], @penultimate_frame, score_and_bonus) when is_spare(r1, r2),
    do: {:active, @pin_max, @max_frames, score_and_bonus |> add_score(r1, r2)}

  # strikes
  defp game_state([@pin_max, b1, b2], @penultimate_frame, score_and_bonus),
    do:
      {:game_over,
       score_and_bonus |> add_score(@pin_max) |> add_bonus_score(b1) |> add_bonus_score(b2)}

  defp game_state([@pin_max, @pin_max], @penultimate_frame, score_and_bonus),
    do:
      {:active, @pin_max, @max_frames,
       score_and_bonus |> add_score(@pin_max) |> add_bonus_score(@pin_max)}

  defp game_state([@pin_max, b1], @penultimate_frame, score_and_bonus),
    do:
      {:active, @pin_max - b1, @max_frames,
       score_and_bonus |> add_score(@pin_max) |> add_bonus_score(b1)}

  defp game_state([@pin_max], @penultimate_frame, score_and_bonus),
    do: {:active, @pin_max, @max_frames, score_and_bonus |> add_score(@pin_max)}

  # no bonus
  defp game_state([r1, r2], @penultimate_frame, score_and_bonus),
    do: {:game_over, score_and_bonus |> add_score(r1, r2)}

  # last rolls in regular frames
  defp game_state([@pin_max], frames, score_and_bonus),
    do: {:active, @pin_max, frames + 1, score_and_bonus |> add_score(@pin_max)}

  defp game_state([r1, r2], frames, score_and_bonus) when is_spare(r1, r2),
    do: {:active, @pin_max, frames + 1, score_and_bonus |> add_score(r1, r2)}

  defp game_state([r1], frames, score_and_bonus),
    do: {:active, @pin_max - r1, frames + 1, score_and_bonus |> add_score(r1)}

  defp game_state([], frames, score_and_bonus),
    do: {:active, @pin_max, frames + 1, score_and_bonus}

  # more rolls to go - score and recurse
  defp game_state([@pin_max | rolls = [b1, b2 | _]], frames, score_and_bonus),
    do:
      game_state(
        rolls,
        frames + 1,
        score_and_bonus |> add_score(@pin_max) |> add_bonus_score(b1) |> add_bonus_score(b2)
      )

  defp game_state([@pin_max | rolls = [b1 | _]], frames, score_and_bonus),
    do:
      game_state(
        rolls,
        frames + 1,
        score_and_bonus |> add_score(@pin_max) |> add_bonus_score(b1)
      )

  defp game_state([r1, r2 | rolls = [b1 | _]], frames, score_and_bonus) when is_spare(r1, r2),
    do: game_state(rolls, frames + 1, score_and_bonus |> add_score(r1, r2) |> add_bonus_score(b1))

  defp game_state([r1, r2 | rolls], frames, score_and_bonus),
    do: game_state(rolls, frames + 1, score_and_bonus |> add_score(r1, r2))

  @spec add_score({valid_points, bonus_rolls}, Integer.t()) :: {valid_points, bonus_rolls}
  defp add_score({score, b}, @pin_max), do: {score + @pin_max, b} |> add_bonus_rolls(2)
  defp add_score({score, b}, r), do: {score + r, b}

  defp add_score(score_and_bonus, r1, r2) when is_spare(r1, r2),
    do: score_and_bonus |> add_score(r1) |> add_score(r2) |> add_bonus_rolls(1)

  defp add_score(score_and_bonus, r1, r2), do: score_and_bonus |> add_score(r1) |> add_score(r2)

  @spec add_bonus_score({valid_points, bonus_rolls}, Integer.t()) :: {valid_points, bonus_rolls}
  defp add_bonus_score({score, 0}, _r), do: {score, 0}
  defp add_bonus_score({score, b}, r), do: {score + r, b - 1}

  @spec add_bonus_rolls({valid_points, bonus_rolls}, Integer.t()) :: {valid_points, bonus_rolls}
  defp add_bonus_rolls({score, b}, n), do: {score, b + n}
end
