class BankAccount
    # Initialize instance variables
    def initialize
      @balance = 0
      @status = 'uninitialized'
    end
  
    # Open the bank account
    def open
      check_status('open')
      @balance = 0
      @status = 'open'
    end
  
    # Close the bank account
    def close
      check_status('close')
      @status = 'closed'
    end
  
    # Deposit an amount into the bank account
    def deposit(amount)
      check_status
      check_amount(amount)
      @balance += amount
    end
  
    # Withdraw an amount from the bank account
    def withdraw(amount)
      check_status
      check_amount(amount)
      check_balance(amount)
      @balance -= amount
    end
  
    # Get the current balance of the bank account
    def balance
      check_status
      @balance
    end
  
    private
  
    # Check the status of the bank account before performing operations
    def check_status(operation = nil)
      if (operation.nil? && ['uninitialized', 'closed'].include?(@status)) ||
         (operation == 'open' && !['uninitialized', 'closed'].include?(@status)) ||
         (operation == 'close' && @status != 'open')
        raise ArgumentError, "Invalid operation: #{@status}"
      end
    end
  
    # Check if there are sufficient funds for withdrawal
    def check_balance(amount)
      raise ArgumentError, "Insufficient funds" if @balance < amount
    end
  
    # Check if the amount is valid (non-negative)
    def check_amount(amount)
      raise ArgumentError, "Invalid amount" if amount < 0
    end
  
  end
