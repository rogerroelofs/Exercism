class Yacht
    RULES = {
        'yacht' => lambda { |dice| dice.uniq.length == 1 ? 50 : 0 },
        'little straight' => lambda { |dice| dice.sort == [1, 2, 3, 4, 5] ? 30 : 0 },
        'big straight' => lambda { |dice| dice.sort == [2, 3, 4, 5, 6] ? 30 : 0 },
        'full house' => lambda { |dice| score_full_house(dice) },
        'four of a kind' => lambda { |dice| score_four_of_a_kind(dice) },
        'choice' => lambda { |dice| dice.sum },
        'ones' => lambda { |dice| score_kind(dice, 1) },
        'twos' => lambda { |dice| score_kind(dice, 2) },
        'threes' => lambda { |dice| score_kind(dice, 3) },
        'fours' => lambda { |dice| score_kind(dice, 4) },
        'fives' => lambda { |dice| score_kind(dice, 5) },
        'sixes' => lambda { |dice| score_kind(dice, 6) }
    }

    def initialize(dice, type)
        @dice = dice
        @type = type
    end

    def score
        if RULES.has_key?(@type)
            RULES[@type].call(@dice)
        else
            0
        end
    end

    private

    def self.score_full_house(dice)
        return 0 if dice.uniq.length != 2
        return 0 if dice.count(dice[0]) != 2 && dice.count(dice[0]) != 3
        dice.sum
    end

    def self.score_four_of_a_kind(dice)
        return dice[0] * 4 if dice.count(dice[0]) >= 4
        return dice[1] * 4 if dice.count(dice[1]) >= 4
        0
    end

    def self.score_kind(dice, kind)
        dice.count(kind) > 0 ? dice.count(kind) * kind : 0
    end
end