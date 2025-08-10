defmodule GuessingGame do
  def compare(_secret_number) do
    "Make a guess"
  end

  def compare(secret_number, guess) do
    cond do
      guess == :no_guess -> "Make a guess"
      guess == secret_number -> "Correct"
      guess + 1 == secret_number -> "So close"
      guess - 1 == secret_number -> "So close"
      guess > secret_number -> "Too high"
      guess < secret_number -> "Too low"
    end
  end
end
