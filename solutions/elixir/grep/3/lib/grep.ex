defmodule Grep do
  @type line() :: {String.t(), Integer.t()}
  @type flags() :: [String.t()]

  @spec grep(String.t(), flags(), [String.t()]) :: String.t()
  def grep(pattern, flags, files) do
    flags = if length(files) > 1, do: ["-f" | flags], else: flags
    output = files
    |> Enum.map(&read_files/1)
    |> Enum.map(fn {file, contents} ->
      {file, find_matches(pattern, contents, flags)}
    end)
    |> Enum.filter(fn {_file, matches} -> length(matches) > 0 end)
    # [{"iliad.txt", [{"Of Atreus, Agamemnon, King of men.", 9}]}]
    |> Enum.map(fn {file, matches} -> output(file, matches, flags) end)
    |> List.flatten
    |> Enum.join("\n")
    if String.length(output) > 0, do: "#{output}\n", else: output
  end

  @spec read_files(String.t()) :: {String.t(), [String.t()]}
  defp read_files(file) do
    {status, contents} = File.read(file)
    case status do
      :ok -> {file, String.split(contents, "\n", trim: true)}
      :error -> fn ->
        IO.puts(:stderr, contents)
        {file, [""]}
      end
    end
  end

  @spec find_matches(String.t(), [String.t()], flags()) :: [line()]
  defp find_matches(pattern, lines, flags) do
    {:ok, regex} = case {Enum.member?(flags, "-i"), Enum.member?(flags, "-x")} do
      {true, true} -> Regex.compile("^#{pattern}$", "i")
      {true, false} -> Regex.compile(pattern, "i")
      {false, true} -> Regex.compile("^#{pattern}$")
      _ -> Regex.compile(pattern)
    end
    lines = Enum.with_index(lines, 1)
    case Enum.member?(flags, "-v") do
      true -> Enum.reject(lines, &check_line(&1, regex))
      false -> Enum.filter(lines, &check_line(&1, regex))
    end
  end

  @spec check_line(line(), Regex.t()) :: boolean()
  defp check_line({line, _}, regex), do: String.match?(line, regex)

  @spec output(String.t(), [line()], flags()) :: [String.t()]
  defp output(file, lines, flags) do
    if Enum.member?(flags, "-l") do
      file
    else
      lines = if Enum.member?(flags, "-n") do
        Enum.map(lines, fn {line, number} ->
          "#{number}:#{line}"
        end)
      else
        Enum.map(lines, fn {line, _number} -> "#{line}" end)
      end
      lines = if Enum.member?(flags, "-f") do
        Enum.map(lines, fn line -> "#{file}:#{line}" end)
      else
        lines
      end
      lines
    end
  end
end
