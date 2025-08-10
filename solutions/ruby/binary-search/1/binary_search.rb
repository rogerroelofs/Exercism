class BinarySearch
    def initialize(haystack)
        @haystack = haystack
    end

    def search_for(needle)
        low = 0
        high = @haystack.length - 1

        while low <= high
            mid = (low + high) / 2
            mid_val = @haystack[mid]

            if mid_val < needle
                low = mid + 1
            elsif mid_val > needle
                high = mid - 1
            else
                return mid
            end
        end

        return nil
    end
end