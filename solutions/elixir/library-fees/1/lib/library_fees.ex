defmodule LibraryFees do
  @spec datetime_from_string(String.t) :: NaiveDateTime.t()
  def datetime_from_string(string) do
    NaiveDateTime.from_iso8601!(string)
  end

  @spec before_noon?(NaiveDateTime.t) :: boolean
  def before_noon?(datetime) do
    datetime.hour < 12
  end

  @spec return_date(NaiveDateTime.t) :: Date.t
  def return_date(checkout_datetime) do
    before = before_noon?(checkout_datetime)
    days = case (before) do
      true -> 28
      false -> 29
    end
    NaiveDateTime.add(checkout_datetime, (days * secs_per_day()), :second)
    |> NaiveDateTime.to_date
  end

  @spec days_late(Date.t, NaiveDateTime.t) :: integer
  def days_late(planned_return_date, actual_return_datetime) do
    act_date = NaiveDateTime.to_date(actual_return_datetime)
    days = Date.diff(act_date, planned_return_date)
    cond do
      days <= 0 -> 0
      true -> days
    end
  end

  @spec monday?(NaiveDateTime.t) :: boolean
  def monday?(datetime) do
    day = NaiveDateTime.to_date(datetime)
      |> Date.day_of_week
    day == 1
  end

  def calculate_late_fee(checkout, return, rate) do
    return = datetime_from_string(return)
    planned = return_date(datetime_from_string(checkout))
    days = days_late(planned, return)
    cond do
      monday?(return) -> trunc(rate * days / 2)
      true -> trunc(rate * days)
    end
  end

  defp secs_per_day(), do: 24 * 3600
end
