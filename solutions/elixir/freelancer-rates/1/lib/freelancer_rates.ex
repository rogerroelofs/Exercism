defmodule FreelancerRates do
  def daily_rate(hourly_rate) do
    hourly_rate * 8.0
  end

  def apply_discount(before_discount, discount) do
    before_discount - before_discount * (discount / 100)
  end

  def monthly_rate(hourly_rate, discount) do
    daily_rate(hourly_rate) * 22
      |> apply_discount(discount)
      |> ceil
      |> trunc
  end

  def days_in_budget(budget, hourly_rate, discount) do
    dr = daily_rate(hourly_rate) |> apply_discount(discount)
    if dr > 0 do
      Float.floor(budget / dr, 1)
    end
  end
end
