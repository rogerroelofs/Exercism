class LogLineParser
  def initialize(line)
    @line = line
    @message = line.split(':', 2)[1].strip if line.include?(':')
    @raw_level = line.split(':', 2)[0].gsub(/[\[\]:]/, '').downcase if line.include?(':')
  end

  def message
    @message
  end

  def log_level
    @raw_level
  end

  def reformat
    "#{message} (#{log_level})"
  end
end
