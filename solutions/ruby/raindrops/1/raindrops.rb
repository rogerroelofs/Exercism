=begin
Write your code for the 'Raindrops' exercise in this file. Make the tests in
`raindrops_test.rb` pass.

To get started with TDD, see the `README.md` file in your
`ruby/raindrops` directory.
=end

class Raindrops
    @sounds = {
        3 => "Pling",
        5 => "Plang",
        7 => "Plong"
    }

    def self.convert(num)
        sounds = @sounds
            .map {|factor, sound| sound if (num % factor == 0)}
            .join ''

        sounds == '' ? num.to_s : sounds
    end
end