class Brackets
    def self.paired?(str)
        stack = []
        str.each_char do |c|
            case c
            when '{', '[', '('
                stack.push(c)
            when '}'
                return false if stack.pop != '{'
            when ']'
                return false if stack.pop != '['
            when ')'
                return false if stack.pop != '('
            end
        end
        stack.empty?
    end
end