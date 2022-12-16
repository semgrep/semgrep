class ParseError < Exception
  def initialize(input, line, pos)
    super "Could not parse '#{input}' at line #{line}, position #{pos}"
  end
end

raise ParseError.new("Foo", 3, 9)
