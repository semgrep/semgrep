def use_hello
  yield "hello"
end

# Invoke the preceding method, passing it a block.
use_hello {|string| puts string}  # => 'hello'
