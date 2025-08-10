=begin
Write your code for the 'Circular Buffer' exercise in this file. Make the tests in
`circular_buffer_test.rb` pass.

To get started with TDD, see the `README.md` file in your
`ruby/circular-buffer` directory.
=end

class CircularBuffer
    class BufferEmptyException < Exception; end
    class BufferFullException < Exception; end

    def initialize(length)
        @buffer = Array.new(length)
        clear
    end

    def write(value)
        raise BufferFullException.new if @count == @buffer.length
        @buffer[@head] = value
        @head = (@head + 1) % @buffer.length
        @count += 1
    end

    def write!(value)
        read if @count >= @buffer.length
        write value
    end

    def read()
        raise BufferEmptyException.new if @count == 0
        value = @buffer[@tail]
        @tail = (@tail + 1) % @buffer.length
        @count -= 1
        value
    end

    def clear
        @count = @head = @tail = 0
    end
end