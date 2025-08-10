defmodule TwoBucket do
  defstruct [:bucket_one, :bucket_two, :moves]
  @type t :: %TwoBucket{bucket_one: integer, bucket_two: integer, moves: integer}

  @doc """
  Find the quickest way to fill a bucket with some amount of water from two buckets of specific sizes.
  """
  @spec measure(
          size_one :: integer,
          size_two :: integer,
          goal :: integer,
          start_bucket :: :one | :two
        ) :: {:ok, TwoBucket.t()} | {:error, :impossible}
  def measure(size_one, size_two, goal, start_bucket) do
    {first, second} =
      case start_bucket do
        :one -> {{0, size_one}, {0, size_two}}
        :two -> {{0, size_two}, {0, size_one}}
      end

    do_measure(first, second, goal, 0)
    |> maybe_swap_bucket(start_bucket)
  end

  # error cases - impossible to reach the goal
  defp do_measure({_, size_one}, {_, size_two}, goal, _)
       when size_one < goal and size_two < goal,
       do: {:error, :impossible}

  defp do_measure({size_one, size_one}, {size_two, size_two}, goal, _)
       when size_one != goal and size_two != goal,
       do: {:error, :impossible}

  # first step, both buckets are empty
  defp do_measure({0, size_one}, {0, goal}, goal, _) do
    {:ok, %TwoBucket{bucket_one: size_one, bucket_two: goal, moves: 2}}
  end

  # bucket one is at goal
  defp do_measure({goal, _}, {in_two, _}, goal, step) do
    {:ok, %TwoBucket{bucket_one: goal, bucket_two: in_two, moves: step}}
  end

  # bucket two is at goal
  defp do_measure({in_one, _}, {goal, _}, goal, step) do
    {:ok, %TwoBucket{bucket_one: in_one, bucket_two: goal, moves: step}}
  end

  # fill first bucket
  defp do_measure({0, size_one}, {in_two, size_two}, goal, step) do
    do_measure({size_one, size_one}, {in_two, size_two}, goal, step + 1)
  end

  # emptying second bucket
  defp do_measure({in_one, size_one}, {size_two, size_two}, goal, step) do
    do_measure({in_one, size_one}, {0, size_two}, goal, step + 1)
  end

  # pouring first bucket into the second bucket
  defp do_measure({in_one, size_one}, {in_two, size_two}, goal, step) do
    value = min(in_one, size_two - in_two)
    do_measure({in_one - value, size_one}, {in_two + value, size_two}, goal, step + 1)
  end

  # swap buckets if start bucket is :two
  defp maybe_swap_bucket({:ok, t}, :two) do
    {:ok, %TwoBucket{bucket_one: t.bucket_two, bucket_two: t.bucket_one, moves: t.moves}}
  end

  defp maybe_swap_bucket(result, _), do: result
end
