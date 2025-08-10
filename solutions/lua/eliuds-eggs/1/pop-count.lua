local PopCount = {}

function PopCount.egg_count(number)
    local sum = 0
    while number>0 do
        rest=math.fmod(number,2)
        sum = sum + rest
        number=(number-rest)/2
    end
    return sum
end

return PopCount
