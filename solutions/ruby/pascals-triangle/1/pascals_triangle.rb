class Triangle
    def initialize(n)
        @n = n
    end

    def rows
        [] if @n == 0
        triangle = [[1]]
        (@n - 1).times do
            triangle << next_row(triangle.last)
        end
        triangle
    end

    private
    def next_row(row)
        prev = [0].concat(row, [0])
        prev.each_cons(2).map { |a, b| a + b }
    end
end