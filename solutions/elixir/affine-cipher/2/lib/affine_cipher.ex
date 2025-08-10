defmodule AffineCipher do
  @typedoc """
  A type for the encryption key
  """
  @type key() :: %{a: integer, b: integer}

  @m 26

  @doc """
  Encode an encrypted message using a key
  """
  @spec encode(key :: key(), message :: String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def encode(%{a: a, b: b}, message) do
    if coprime(a) do
      {:ok,
       message
       |> String.downcase()
       |> String.replace(~r/[^a-z0-9]+/, "")
       |> String.to_charlist()
       |> Enum.map(fn char ->
         if char >= ?0 and char <= ?9,
           do: char,
           else: rem(a * (char - ?a) + b, @m) + ?a
       end)
       |> Enum.chunk_every(5)
       |> Enum.map(&to_string/1)
       |> Enum.join(" ")}
    else
      {:error, "a and m must be coprime."}
    end
  end

  @doc """
  Decode an encrypted message using a key
  """
  @spec decode(key :: key(), message :: String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def decode(%{a: a, b: b}, encrypted) do
    if coprime(a) do
      {:ok,
       encrypted
       |> String.replace(" ", "")
       |> String.to_charlist()
       |> Enum.map(fn char ->
         if char >= ?0 and char <= ?9,
           do: char,
           else: Integer.mod(mmi(a) * (char - ?a - b), @m) + ?a
       end)
       |> to_string}
    else
      {:error, "a and m must be coprime."}
    end
  end

  defp coprime(a) do
    Integer.gcd(a, @m) == 1
  end

  defp mmi(a) do
    Enum.find(1..@m, fn n -> Integer.mod(a * n, @m) == 1 end)
  end
end
