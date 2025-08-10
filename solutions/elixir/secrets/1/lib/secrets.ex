use Bitwise, only_operators: true

defmodule Secrets do
  def secret_add(secret) do
    # fn (arg) -> arg + secret end
    &(&1 + secret)
  end

  def secret_subtract(secret) do
    # fn (arg) -> arg - secret end
    &(&1 - secret)
  end

  def secret_multiply(secret) do
    fn (arg) -> arg * secret end
    # &(&1 * secret)
  end

  def secret_divide(secret) do
    fn (arg) -> div arg, secret end
    # &(div &1, secret)
  end

  def secret_and(secret) do
    fn (arg) -> Bitwise.band(arg, secret) end
  end

  def secret_xor(secret) do
    fn (arg) -> Bitwise.bxor(arg, secret) end
  end

  def secret_combine(secret_function1, secret_function2) do
    fn (arg) -> arg |> secret_function1.() |> secret_function2.() end
  end
end
