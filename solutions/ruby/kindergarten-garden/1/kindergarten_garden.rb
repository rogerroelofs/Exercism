class Garden
    STUDENTS = %w(Alice Bob Charlie David Eve Fred Ginny Harriet Ileana Joseph Kincaid Larry)
    PLANTS = { 'G' => :grass, 'C' => :clover, 'R' => :radishes, 'V' => :violets }

    def initialize(diagram)
        @diagram = diagram.split("\n").map { |row| row.split('') }
    end

    def method_missing(method_name)
        if STUDENTS.include?(method_name.to_s.capitalize)
            plants = @diagram.map { |row| row[STUDENTS.index(method_name.to_s.capitalize) * 2, 2] }
            plants.flatten.map { |plant| PLANTS[plant] }
        end
    end
end