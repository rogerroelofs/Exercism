return function(score_list)
    local fn = {}
    
    function fn:scores(self)
      return score_list
    end
    function fn:latest(self)
      return score_list[#score_list]
    end
    function fn:personal_best(self)
      return math.max(table.unpack(score_list))
    end
    function fn:personal_top_three(self)
      local sorted = { table.unpack(score_list) }
      table.sort(sorted, function(a, b) return a > b end)
      while #sorted > 3 do table.remove(sorted) end
      return sorted
    end

    return fn
end