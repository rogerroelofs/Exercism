defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  For this example, assume all input are strings, that are all uppercase letters.
  It should also be able to reconstruct the data into its original form.
  "2A3B4C" => "AABBBCCCC"
  """
  @spec encode(String.t()) :: String.t()
  def encode(string) do
    string
    |> String.codepoints
    |> Enum.chunk_by(&(&1))
    |> Enum.map(&encode_one/1)
    |> Enum.join
  end

  @spec decode(String.t()) :: String.t()
  def decode(string) do
    Regex.scan(~r{(\d*)(.)}, string)
    |> Enum.map(&decode_one/1)
    |> Enum.join()
  end


  defp encode_one([char]) do
    char
  end
  defp encode_one(char_list) do
    "#{length(char_list)}#{List.first(char_list)}"
  end

  defp decode_one([_, "", char]) do
    char
  end
  defp decode_one([_, count, char]) do
    String.duplicate(char, String.to_integer(count))
  end

end
