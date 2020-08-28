# In an object instance variable (denoted with '@'), remember a block.
def remember(&a_block)
  @block = a_block
end

# Invoke the preceding method, giving it a block that takes a name.
remember {|name| puts "Hello, #{name}!"}

# Call the closure (note that this happens not to close over any free variables):
@block.call('Jon')   # => "Hello, Jon!"
