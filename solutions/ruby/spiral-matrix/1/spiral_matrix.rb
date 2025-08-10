class SpiralMatrix
    attr_reader :matrix
    
    def initialize(dim)
        @matrix = Array.new(dim) { Array.new(dim) }
        startrow = startcol = 0
        endrow = endcol = dim - 1
        num = 1
        while startrow <= endrow && startcol <= endcol
            startcol.upto(endcol) do |i|
                @matrix[startrow][i] = num
                num += 1
            end
            startrow += 1
            startrow.upto(endrow) do |i|
                @matrix[i][endcol] = num
                num += 1
            end
            endcol -= 1
            endcol.downto(startcol) do |i|
                @matrix[endrow][i] = num
                num += 1
            end
            endrow -= 1
            endrow.downto(startrow) do |i|
                @matrix[i][startcol] = num
                num += 1
            end
            startcol += 1
        end
    end
end