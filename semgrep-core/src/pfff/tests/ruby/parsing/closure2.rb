def create_set_and_get(initial_value=0) # note the default value of 0
  closure_value = initial_value
  [ Proc.new {|x| closure_value = x}, Proc.new { closure_value } ]
end

setter, getter = create_set_and_get  # returns two values
setter.call(21)
getter.call      # => 21

# Parameter variables can also be used as a binding for the closure,
# so the preceding can be rewritten as:

def create_set_and_get(closure_value=0)
  [ proc {|x| closure_value = x } , proc { closure_value } ]
end
