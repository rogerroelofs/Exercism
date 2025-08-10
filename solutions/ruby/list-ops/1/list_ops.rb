=begin
Write your code for the 'List Ops' exercise in this file. Make the tests in
`list_ops_test.rb` pass.

To get started with TDD, see the `README.md` file in your
`ruby/list-ops` directory.
=end

module ListOps
    def self.arrays(a)
        count = 0
        a.each { count += 1 }
        count
    end

    def self.reverser(a)
        res = []
        res << a.pop until a.empty?
        res
    end

    def self.concatter(a1, a2)
        res = []
        res << a1 << a2
        res.flatten
    end

    def self.mapper(a)
        res = []
        a.each { |n| res << yield(n) }
        res
    end

    def self.filterer(a, &block)
        res = []
        a.each { |n| res << n if block.call(n) }
        res
    end

    def self.sum_reducer(a)
        sum = 0
        sum += a.pop until a.empty?
        sum
    end

    def self.factorial_reducer(a)
        factorial = 1
        factorial *= a.pop until a.empty?
        factorial
    end
end