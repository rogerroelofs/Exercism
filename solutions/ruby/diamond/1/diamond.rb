class Diamond
    def self.make_diamond(character)
      width = (character.ord - 'A'.ord) * 2 + 1
  
      front = ('A'..character).map { |letter| build_line(letter, width) }
  
      (front + front[0...-1].reverse).join("\n") + "\n"
    end
  
    def self.build_line(letter, width)
      inner_width = (letter.ord - 'A'.ord) * 2 - 1
      outer_width = (width - (letter.ord - 'A'.ord) * 2 - 1) / 2
      letter_str = letter
      outer_pad = ' ' * outer_width
  
      content = inner_width < 1 ? letter_str : "#{letter_str}#{' ' * inner_width}#{letter_str}"
  
      "#{outer_pad}#{content}#{outer_pad}"
    end
end