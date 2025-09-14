class AssemblyLine
  BASE_PRODUCTION_RATE = 221

  def initialize(speed)
    @speed = speed
  end

  def production_rate_per_hour
    success_rate = 1.0
    success_rate = 0.9 if @speed.between?(5, 8)
    success_rate = 0.8 if @speed == 9
    success_rate = 0.77 if @speed == 10
    @speed * BASE_PRODUCTION_RATE * success_rate
  end

  def working_items_per_minute
    (production_rate_per_hour / 60).floor
  end
end
