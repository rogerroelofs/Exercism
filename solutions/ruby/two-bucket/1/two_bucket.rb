class TwoBucket
    def initialize(bucket_one, bucket_two, goal, start_bucket)
        @buckets = [{name: "one", size: bucket_one, current: 0}, {name: "two", size: bucket_two, current: 0}]
        @buckets = @buckets.reverse if start_bucket == "two"
        @goal = goal
        @start_bucket = start_bucket
        @moves = 0
        solve
    end

    def solve
        solved_bucket = nil
        until @buckets.find { |b| b[:current] == @goal } != nil
            @moves += 1
            first, second = @buckets
            if first[:current].zero?
                first[:current] = first[:size]
            elsif second[:size] == @goal
                second[:current] = second[:size]
            elsif second[:current] < second[:size]
                first, second = transfer(first, second)
            else
                second[:current] = 0
            end
            @buckets = [first, second]
        end
    end

    def moves
        @moves
    end

    def goal_bucket
        @buckets.find { |b| b[:current] == @goal }[:name]
    end

    def other_bucket
        @buckets.find { |b| b[:current] != @goal }[:current]
    end

    #private
    def transfer(from, to)
        if from[:current] <= to[:size] - to[:current]
            to[:current] += from[:current]
            from[:current] = 0
        else
            from[:current] -= to[:size] - to[:current]
            to[:current] = to[:size]
        end
        [from, to]
    end
end