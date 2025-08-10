
class Luhn
    def self.valid?(input)
        input = input.gsub(/\s+/, "")
        return false if input.length <= 1 || input.match?(/\D/)

        (input
            .chars.map(&:to_i)
            .reverse.map.with_index do |num, index|
                if index.odd?
                    num *= 2
                    num -= 9 if num >= 10
                end
                num
            end
            .sum % 10).zero?
    end
end