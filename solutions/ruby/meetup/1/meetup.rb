require 'date'

class Meetup
    def initialize(month, year)
        @month = month
        @year = year
    end

    def day(dayOfWeek, type)
        firstDay = Date.new(@year, @month, 1)
        lastDay = Date.new(@year, @month, -1)
        case type
        when :first
            firstDay.upto(lastDay) do |date|
                if date.strftime("%A").downcase == dayOfWeek.to_s
                    return date
                end
            end
        when :second
            firstDay.upto(lastDay) do |date|
                if date.strftime("%A").downcase == dayOfWeek.to_s
                    return date + 7
                end
            end
        when :third
            firstDay.upto(lastDay) do |date|
                if date.strftime("%A").downcase == dayOfWeek.to_s
                    return date + 14
                end
            end
        when :fourth
            firstDay.upto(lastDay) do |date|
                if date.strftime("%A").downcase == dayOfWeek.to_s
                    return date + 21
                end
            end
        when :teenth
            firstDay.upto(lastDay) do |date|
                if date.strftime("%A").downcase == dayOfWeek.to_s && date.day >= 13 && date.day <= 19
                    return date
                end
            end
        when :last
            lastDay.downto(firstDay) do |date|
                if date.strftime("%A").downcase == dayOfWeek.to_s
                    return date
                end
            end
        end
    end
end