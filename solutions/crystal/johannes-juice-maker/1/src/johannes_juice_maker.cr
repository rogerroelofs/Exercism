class JuiceMaker

    @fluid = 0
    @running = false

    def self.debug_light_on?
        true
    end

    def initialize(amount)
        @running = false
        @fluid = amount
    end

    def start
        @running = true
    end

    def running?
        @running
    end

    def add_fluid(fluid)
        @fluid += fluid
    end

    def stop(minutes)
        @running = false
        @fluid -= minutes * 5
    end
end