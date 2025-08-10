defmodule DancingDots.Animation do
  @type dot :: DancingDots.Dot.t()
  @type opts :: keyword
  @type error :: any
  @type frame_number :: pos_integer

  @callback init(opts :: opts) :: {:ok, opts} | {:error, error}
  @callback handle_frame(dot :: dot, frame_number :: frame_number, opts :: opts) :: dot
  defmacro __using__(_) do
    quote do
      @behaviour DancingDots.Animation
      def init(opts), do: {:ok, opts}
      defoverridable init: 1
    end
  end
end

defmodule DancingDots.Flicker do
  use DancingDots.Animation

  @impl DancingDots.Animation
  def handle_frame(dot, frame_number, _opts) when rem(frame_number, 4) == 0 do
    %{dot | opacity: dot.opacity / 2}
  end

  def handle_frame(dot, _frame_number, _opts), do: dot
end

defmodule DancingDots.Zoom do
  @behaviour DancingDots.Animation
  @impl DancingDots.Animation
  def init(velocity: n) when is_number(n), do: {:ok, [velocity: n]}

  def init(opts),
    do:
      {:error,
       "The :velocity option is required, and its value must be a number. Got: #{inspect(Keyword.get(opts, :velocity))}"}

  @impl DancingDots.Animation
  def handle_frame(dot, frame_number, velocity: n) do
    %{dot | radius: dot.radius + (frame_number - 1) * n}
  end
end
