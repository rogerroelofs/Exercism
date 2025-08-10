class Anagram
    def initialize(word)
        @word = word.downcase
        @sorted_word = @word.chars.sort
    end

    def match(words)
        words.select { |w| anagram?(w) }
    end

    private
    def anagram?(word)
        return false if word.downcase == @word
        word.downcase.chars.sort == @sorted_word
    end
end