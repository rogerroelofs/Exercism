defmodule Meetup do
  @moduledoc """
  Calculate meetup dates.
  """

  @type weekday ::
          :monday
          | :tuesday
          | :wednesday
          | :thursday
          | :friday
          | :saturday
          | :sunday

  @type schedule :: :first | :second | :third | :fourth | :last | :teenth

  @weekdays [:monday, :tuesday, :wednesday, :thursday, :friday, :saturday, :sunday] |> Enum.with_index(1) |> Enum.into(%{})

  @schedule_range %{
    first: 1..7,
    second: 8..14,
    third: 15..21,
    fourth: 22..28,
    teenth: 13..19,
  }
  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the meetup date should
  fall.
  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: :calendar.date()
  def meetup(year, month, weekday, schedule) do
    weekday = @weekdays[weekday]
    Map.get(@schedule_range, schedule, last_seven(year, month))
    |> Enum.map(&(Date.from_erl!({year, month, &1})))
    |> Enum.filter(&(Date.day_of_week(&1) == weekday))
    |> List.first
  end

  defp last_seven(year, month) do
    last_day = Date.from_erl!({year, month, 1}) |> Date.end_of_month
    first_day = last_day.day - 6
    first_day..last_day.day
  end
end
