module RunLengthEncoding
    def self.encode(input)
        input.chars.chunk_while { |prev, curr| prev == curr }
        .map { |chunk| "#{chunk.size if chunk.size > 1}#{chunk.first}" }
        .join
    end

    def self.decode(input)
        input.scan(/(\d*)(\D)/).map do |count, char| 
            char * (count.empty? ? 1 : count.to_i) 
        end.join
    end
end