defmodule RationalNumbers do
  @type rational :: {integer, integer}

  @doc """
  Add two rational numbers
  """
  @spec add(a :: rational, b :: rational) :: rational
  def add({a1, b1}, {a2, b2}) do
    {(a1 * b2 + a2 * b1), (b1 * b2)}
    |> reduce
  end

  @doc """
  Subtract two rational numbers
  """
  @spec subtract(a :: rational, b :: rational) :: rational
  def subtract({a1, b1}, {a2, b2}) do
    {(a1 * b2 - a2 * b1), (b1 * b2)}
    |> reduce
  end

  @doc """
  Multiply two rational numbers
  """
  @spec multiply(a :: rational, b :: rational) :: rational
  def multiply({a1, b1}, {a2, b2}) do
    {(a1 * a2), (b1 * b2)}
    |> reduce
  end

  @doc """
  Divide two rational numbers
  """
  @spec divide_by(num :: rational, den :: rational) :: rational
  def divide_by({a1, b1}, {a2, b2}) do
    {(a1 * b2), (a2 * b1)}
    |> reduce
  end

  @doc """
  Absolute value of a rational number
  """
  @spec abs(a :: rational) :: rational
  def abs({a, b}) do
    {Kernel.abs(a), Kernel.abs(b)}
    |> reduce
  end

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(a :: rational, n :: integer) :: rational
  def pow_rational({a, b}, n) when n >= 0 do
    {:math.pow(a,n) |> round, :math.pow(b,n) |> round}
    |> reduce
  end
  def pow_rational({a, b}, n) when n < 0 do
    m = Kernel.abs(n)
    {:math.pow(b,m) |> round, :math.pow(a,m) |> round}
    |> reduce
  end

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(x :: integer, n :: rational) :: float
  def pow_real(x, {a, b}) do
    x ** (a/b)
  end

  @doc """
  Reduce a rational number to its lowest terms
  """
  @spec reduce(a :: rational) :: rational
  def reduce({0, _b}), do: {0, 1}
  def reduce({a, b}) do
    divisor = gcd(a, b)
    {numerator, denominator} = {div(a, divisor), div(b, divisor)}
    if denominator < 0 do
      {numerator * -1, denominator * -1}
    else
      {numerator, denominator}
    end
  end

  defp gcd(x, 0), do: x
  defp gcd(x, y), do: gcd(y, rem(x,y))
end
