class School
  def initialize
    @roster = {}
  end

  def add(name, grade)
    return false if @roster.values.flatten.include?(name)
    @roster[grade] = [] unless @roster[grade]
    @roster[grade] << name
    true
  end

  def roster
    ret = []
    @roster.sort.to_h.each do |grade, names|
        ret << names.sort
    end
    ret.flatten
  end

  def grade(grade)
    @roster[grade] ? @roster[grade].sort : []
  end

end