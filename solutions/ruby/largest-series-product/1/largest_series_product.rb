class Series
    def initialize(series)
        @series = series
    end
    
    def largest_product(n)
        raise ArgumentError if n > @series.length
        raise ArgumentError if @series.chars.any? { |c| c.match?(/\D/) }
        return 1 if n == 0
        return 0 if @series.empty?
        return @series.chars.map(&:to_i).each_cons(n).map { |a| a.reduce(:*) }.max
    end
end