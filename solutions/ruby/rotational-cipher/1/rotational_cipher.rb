module RotationalCipher
    def self.rotate(string, shift)
        string.chars.map do |char|
        if char.match?(/[a-z]/)
            shift_char(char, shift, 'a')
        elsif char.match?(/[A-Z]/)
            shift_char(char, shift, 'A')
        else
            char
        end
        end.join
    end
    
    def self.shift_char(char, shift, base)
        ((char.ord - base.ord + shift) % 26 + base.ord).chr
    end
end