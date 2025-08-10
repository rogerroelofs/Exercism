local Hamming = {}

function Hamming.compute(a,b)
  local distance = 0

  -- cannot calculate Hamming distance if strings have different sizes
  if #a ~= #b then return -1 end

  for i = 1, #a do
    if a:byte(i,i) ~= b:byte(i,i) then
      distance = distance + 1 
    end
  end

  return distance
end

return Hamming