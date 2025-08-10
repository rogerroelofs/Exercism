class Knapsack
    def initialize(max_weight)
        @max_weight = max_weight
    end
    def max_value(items)
        items = items.select { |item| item.weight <= @max_weight }
        return 0 if items.empty?
        items = items.sort_by { |item| item.value }
        items = items.reverse
        items.each_with_index do |item, index|
            if item.weight <= @max_weight
                return item.value
            end
        end
    end
end