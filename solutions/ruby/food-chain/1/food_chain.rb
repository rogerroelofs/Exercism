
class FoodChain
    def self.animals
        [
            {kind: "fly", verse: "I don't know why she swallowed the fly. Perhaps she'll die."},
            {kind: "spider", verse: "It wriggled and jiggled and tickled inside her."},
            {kind: "bird", verse: "How absurd to swallow a bird!"},
            {kind: "cat", verse: "Imagine that, to swallow a cat!"},
            {kind: "dog", verse: "What a hog, to swallow a dog!"},
            {kind: "goat", verse: "Just opened her throat and swallowed a goat!"},
            {kind: "cow", verse: "I don't know how she swallowed a cow!"}
        ]
    end

    def self.song
        past_animals = []
        song = []
        sequence = animals
        while animal = sequence.shift do
          song << verse(animal, past_animals)
          song << ""
          past_animals.unshift(animal)
        end
        song << "I know an old lady who swallowed a horse."
        song << "She's dead, of course!\n"
        song.flatten.join("\n")
    end

    private
    def self.start(animal_name)
        "I know an old lady who swallowed a #{animal_name}."
    end
      
    def self.combine(curr_animal, prev_animal)
        "She swallowed the #{curr_animal} to catch the #{prev_animal}."
    end
      
    def self.do_spider(first, second)
        first.gsub('.', ' ') + second.gsub('It ', 'that ')
    end

    def self.verse(animal, past_animals)
        lines = Array.new.tap do |l|
            l << start(animal[:kind])
            l << animal[:verse]
            current = animal
            past_animals.each do |a|
              line = combine(current[:kind], a[:kind])
              if a[:kind] == 'spider' then
                line = do_spider(line, a[:verse])
              end
              l << line
              current = a
            end
        end
        lines << "I don't know why she swallowed the fly. Perhaps she'll die." if animal[:kind] != "fly"
        lines
    end
end