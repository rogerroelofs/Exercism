module SavingsAccount
  def self.interest_rate(balance)
    return 3.213 if balance.negative?
    return 0.5 if balance < 1000
    return 1.621 if balance < 5000
    2.475
  end

  def self.annual_balance_update(balance)
    rate = interest_rate(balance)
    if balance.negative?
      balance - (balance * -rate / 100)
    else
      balance + (balance * rate / 100)
    end
  end

  def self.years_before_desired_balance(current_balance, desired_balance)
    years = 0
    while current_balance < desired_balance
      current_balance = annual_balance_update(current_balance)
      years += 1
    end
    years
  end
end
