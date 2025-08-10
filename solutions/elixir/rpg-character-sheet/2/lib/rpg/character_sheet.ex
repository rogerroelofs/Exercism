defmodule RPG.CharacterSheet do
  @spec welcome :: :ok
  def welcome() do
    IO.puts("Welcome! Let's fill out your character sheet together.")
  end

  @spec ask(prompt :: String.t) :: String.t
  defp ask(prompt) do
    IO.gets(prompt) |> String.trim
  end

  @spec ask_name :: String.t
  def ask_name() do
    ask "What is your character's name?\n"
  end

  @spec ask_class :: String.t
  def ask_class() do
    ask "What is your character's class?\n"
  end

  @spec ask_level :: integer
  def ask_level() do
    level = ask "What is your character's level?\n"
    level |> String.to_integer
  end

  @spec run :: %{class: String.t, level: integer, name: String.t}
  def run() do
    welcome()
    IO.inspect(%{
      name: ask_name(),
      class: ask_class(),
      level: ask_level()
    }, label: "Your character")
  end
end
