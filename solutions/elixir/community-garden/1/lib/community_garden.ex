# Use the Plot struct as it is provided
defmodule Plot do
  @enforce_keys [:plot_id, :registered_to]
  defstruct [:plot_id, :registered_to]
end

defmodule CommunityGarden do
  @spec start(GenServer.options()) :: Agent.on_start()
  def start(opts \\ []) do
    Agent.start(fn -> %{idx: 0, data: []} end, opts)
  end

  @spec list_registrations(GenServer.agent()) :: list()
  def list_registrations(pid) do
    Agent.get(pid, & &1.data)
  end

  def register(pid, register_to) do
    state = Agent.get(pid, & &1)
    np = %Plot{plot_id: state.idx + 1, registered_to: register_to}
    Agent.update(pid, fn state -> %{
      idx: state.idx + 1,
      data: [np | state.data]
    } end)
    np
  end

  def release(pid, plot_id) do
    Agent.update(pid, fn state -> %{
      idx: state.idx,
      data: state.data |> Enum.filter(& &1.plot_id != plot_id)
    } end)
  end

  def get_registration(pid, plot_id) do
    registrations = Agent.get(pid, & &1.data) # list_registrations(pid)
    found = registrations |> Enum.filter(fn item ->
      item.plot_id == plot_id
    end)
    case found do
      [] -> {:not_found, "plot is unregistered"}
      _ -> hd(found)
    end
  end
end
