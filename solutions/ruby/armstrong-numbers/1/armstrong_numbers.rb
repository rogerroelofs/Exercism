module ArmstrongNumbers
  def self.include?(number)
    digits = number.to_s.chars
    answer = digits.reduce(0) { |acc, cur| acc + cur.to_i**digits.length }
    answer == number
  end
end
