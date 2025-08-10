defmodule CircularBuffer do
  @moduledoc """
  An API to a stateful process that fills and empties a circular buffer
  """

  @doc """
  Create a new buffer of a given capacity
  """
  @spec new(capacity :: integer) :: {:ok, pid}
  def new(capacity) do
    # read and write ptr are 0
    Agent.start(fn -> {capacity, 0, 0, List.duplicate(nil, capacity)} end)
  end

  @doc """
  Read the oldest entry in the buffer, fail if it is empty
  """
  @spec read(buffer :: pid) :: {:ok, any} | {:error, atom}
  def read(buffer) do
    {_cap, read, write, buf} = Agent.get(buffer, fn x -> x end)

    if read == write and Enum.at(buf, read) |> is_nil do
      {:error, :empty}
    else
      Agent.update(buffer, fn {cap, read, write, buf} ->
        r = if(read == cap - 1, do: 0, else: read + 1)
        {cap, r, write, List.update_at(buf, read, fn _ -> nil end)}
      end)

      {:ok, Enum.at(buf, read)}
    end
  end

  @doc """
  Write a new item in the buffer, fail if is full
  """
  @spec write(buffer :: pid, item :: any) :: :ok | {:error, atom}
  def write(buffer, item) do
    {_cap, read, write, buf} = Agent.get(buffer, fn x -> x end)

    if read == write and Enum.at(buf, read) != nil do
      {:error, :full}
    else
      Agent.update(buffer, fn {cap, read, write, buf} ->
        w = if(write == cap - 1, do: 0, else: write + 1)
        {cap, read, w, List.update_at(buf, write, fn _ -> item end)}
      end)
    end
  end

  @doc """
  Write an item in the buffer, overwrite the oldest entry if it is full
  """
  @spec overwrite(buffer :: pid, item :: any) :: :ok
  def overwrite(buffer, item) do
    {_cap, read, write, buf} = Agent.get(buffer, fn x -> x end)

    if read == write and Enum.at(buf, read) != nil do
      Agent.update(buffer, fn {cap, read, write, buf} ->
        r = if(read == cap - 1, do: 0, else: read + 1)
        w = if(write == cap - 1, do: 0, else: write + 1)
        {cap, r, w, List.update_at(buf, write, fn _ -> item end)}
      end)
    else
      write(buffer, item)
    end
  end

  @doc """
  Clear the buffer
  """
  @spec clear(buffer :: pid) :: :ok
  def clear(buffer) do
    Agent.update(buffer, fn {cap, _, _, _} ->
      # read and write ptr are 0
      {cap, 0, 0, List.duplicate(nil, cap)}
    end)
  end
end
