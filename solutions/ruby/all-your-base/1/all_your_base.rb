class BaseConverter
    def self.convert(inputBase, digits, outputBase)
        throw ArgumentError unless inputBase > 1 && outputBase > 1 && digits.reject {|e| (0...inputBase).include? e}.empty?
        # if no conversion is needed, just return the input
        return digits if (inputBase === outputBase)

        self.from_base10(self.to_base10(digits, inputBase), outputBase)
    end

    private

    def self.to_base10(input, from_base)
        input.reduce(0) {|m, e| m * from_base + e}
    end

    def self.from_base10(base10, to_base)
        output = [base10 % to_base]
        output.unshift base10 % to_base while (base10 /= to_base) > 0
        output
    end
end