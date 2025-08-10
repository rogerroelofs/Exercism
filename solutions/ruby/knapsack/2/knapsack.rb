require 'matrix'

class Item
    attr_reader :weight, :value
    def initialize(weight, value)
        @weight = weight
        @value = value
    end
end

class Knapsack
    def initialize(max_weight)
        @max_weight = max_weight
    end

    def max_value(items)
        dp = Matrix.build(items.length + 1, @max_weight + 1) { 0 }

        (1..items.length).each do |i|
            (1..@max_weight).each do |w|
                item = items[i - 1]
                if item.weight > w
                    dp[i, w] = dp[i - 1, w]
                else
                    dp[i, w] = [dp[i - 1, w], dp[i - 1, w - item.weight] + item.value].max
                end
            end
        end

        dp[items.length, @max_weight]
    end
end
