module Port
  IDENTIFIER = :PALE  # Palermo Port Identifier
  TERMINALS = {
    :OIL123 => :A,
    :GAS674 => :A,
    :CAR942 => :B,
    :CLO315 => :B,
  }.freeze

  def self.get_identifier(city)
    return :ROME if city.strip.downcase == 'rome'
    return :HAMB if city.strip.downcase == 'hamburg'
    return :KIEL if city.strip.downcase == 'kiel'
    raise 'Unknown city'
  end

  def self.get_terminal(ship_identifier)
    TERMINALS[ship_identifier] || (raise 'Unknown ship identifier') 
  end
end
