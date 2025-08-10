defmodule LogLevel do

  @levels ~w(trace debug info warning error fatal unknown)a

  def to_label(level, legacy?) do
    cond do
      legacy? and ((level == 0) or (level == 5)) ->
        :unknown
      level > 6 ->
        :unknown
      true ->
        Enum.at(@levels, level)
    end
  end

  def alert_recipient(level, legacy?) do
    label = to_label(level, legacy?)
    cond do
      (label == :error) or (label == :fatal) ->
        :ops
      legacy? and (label == :unknown) ->
        :dev1
      (label == :unknown) ->
        :dev2
      true ->
        false
    end
  end
end
