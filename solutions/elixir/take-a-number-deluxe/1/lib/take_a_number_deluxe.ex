defmodule TakeANumberDeluxe do
  alias TakeANumberDeluxe.State
  use GenServer
  # Client API

  @spec start_link(keyword()) :: {:ok, pid()} | {:error, atom()}
  def start_link(init_arg) do
    GenServer.start_link(__MODULE__, init_arg)
  end

  @spec report_state(pid()) :: TakeANumberDeluxe.State.t()
  def report_state(machine) do
    GenServer.call(machine, :get)
  end

  @spec queue_new_number(pid()) :: {:ok, integer()} | {:error, atom()}
  def queue_new_number(machine) do
    GenServer.call(machine, :enqueue)
  end

  @spec serve_next_queued_number(pid(), integer() | nil) :: {:ok, integer()} | {:error, atom()}
  def serve_next_queued_number(machine, priority_number \\ nil) do
    GenServer.call(machine, {:dequeue, priority_number})
  end

  @spec reset_state(pid()) :: :ok
  def reset_state(machine) do
    GenServer.cast(machine, :reset)
  end

  # Server callbacks

  @impl GenServer
  def init(opts) do
    timeout = opts[:auto_shutdown_timeout] || :infinity

    case TakeANumberDeluxe.State.new(opts[:min_number], opts[:max_number], timeout) do
      {:ok, state} -> {:ok, state, state.auto_shutdown_timeout}
      {:error, reason} -> {:stop, reason}
    end
  end

  @impl GenServer
  def handle_call(:get, _from, state) do
    {:reply, state, state, state.auto_shutdown_timeout}
  end

  @impl GenServer
  def handle_call(:enqueue, _from, state) do
    case State.queue_new_number(state) do
      {:ok, next_num, new_state} ->
        {:reply, {:ok, next_num}, new_state, state.auto_shutdown_timeout}

      {:error, _} = error ->
        {:reply, error, state, state.auto_shutdown_timeout}
    end
  end

  @impl GenServer
  def handle_call({:dequeue, priority_number}, _from, state) do
    case State.serve_next_queued_number(state, priority_number) do
      {:ok, next_num, new_state} ->
        {:reply, {:ok, next_num}, new_state, state.auto_shutdown_timeout}

      {:error, _} = error ->
        {:reply, error, state, state.auto_shutdown_timeout}
    end
  end

  @impl GenServer
  def handle_cast(:reset, state) do
    case TakeANumberDeluxe.State.new(
           state.min_number,
           state.max_number,
           state.auto_shutdown_timeout
         ) do
      {:ok, new_state} -> {:noreply, new_state, state.auto_shutdown_timeout}
      {:error, _} -> {:noreply, state, state.auto_shutdown_timeout}
    end
  end

  @impl GenServer
  def handle_info(:timeout, state), do: {:stop, :normal, state}

  def handle_info(_, state), do: {:noreply, state, state.auto_shutdown_timeout}
end
