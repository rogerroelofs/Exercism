module Bob
    RESPONSE = {
        whatever: 'Whatever.',
        chill: 'Whoa, chill out!',
        sure: 'Sure.',
        calm: "Calm down, I know what I'm doing!",
        fine: 'Fine. Be that way!'
    }.freeze

    def self.hey(message)
        message = message.strip
        return RESPONSE[:fine] if message.empty?
        return RESPONSE[:calm] if question?(message) && shouting?(message)
        return RESPONSE[:sure] if question?(message)
        return RESPONSE[:chill] if shouting?(message)
        RESPONSE[:whatever]
    end

    private
    def self.question?(message)
        message.end_with?('?')
    end

    def self.shouting?(message)
        cleaned = message.gsub(/[^a-z]/i, '')
        !cleaned.empty? && cleaned.upcase == cleaned
    end
end