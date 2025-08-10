defmodule CaptainsLog do
  @planetary_classes ["D", "H", "J", "K", "L", "M", "N", "R", "T", "Y"]

  @spec random_planet_class :: String.t
  def random_planet_class() do
    Enum.random(@planetary_classes)
  end

  @spec random_ship_registry_number :: Integer.t
  def random_ship_registry_number() do
    "NCC-#{Enum.random(1000..9999)}"
  end

  @spec random_stardate :: float
  def random_stardate() do
    :rand.uniform * 1000 + 41000
  end

  @spec format_stardate(Float.t) :: String.t
  def format_stardate(stardate) do
    case is_float(stardate) do
      true -> "#{stardate |> Float.round(1)}"
      false -> raise ArgumentError, message: "stardate must be a float"
    end
  end


end
