class CollatzConjecture
    def self.steps(count)
        raise ArgumentError if count <= 0
        
        step_count = 0
        
        while count > 1
            count = (count.even?) ? count / 2 : count * 3 + 1
            step_count += 1
        end
        
        step_count
    end
end
