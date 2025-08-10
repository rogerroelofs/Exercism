class Sieve
    def initialize(limit)
        @limit = limit
    end

    def primes
        numbers = (2..@limit)
        primes = []
        while numbers.any?
            prime = numbers.first
            primes << prime
            numbers = numbers.select { |num| num % prime != 0 }
        end
        primes
    end
end