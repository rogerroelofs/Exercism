local ArmstrongNumbers = {}

function ArmstrongNumbers.is_armstrong_number(number)
    local m, sum, digit = string.len(number), 0
    for pos = 1, m do
        digit = tonumber(string.sub(number, pos, pos))
        sum = sum + digit^m
    end
    return sum == number
end

return ArmstrongNumbers
