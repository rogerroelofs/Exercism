defmodule TakeANumber do

  @spec start :: pid
  def start() do
    spawn(fn -> loop(0) end)
  end

  defp loop(num) do
    new_num =
      receive do
        {:report_state, sender_pid} ->
          send(sender_pid, num)
          num
        {:take_a_number, sender_pid} ->
          send(sender_pid, num + 1)
          num + 1
        :stop ->
          nil
        _ ->
          num
      end
    if new_num, do: loop(new_num)
  end
end
