module Isogram
    def self.isogram?(input)
        return true if input.empty?

        counts = input
          .downcase
          .gsub(/[ -]/, '')
          .chars
          .each_with_object(Hash.new(0)) do |char, acc|
            acc[char] += 1
          end
      
        counts.values.max == 1
    end
end