
def test

  # this used to not be parsed as a keyword argument but as a nested call
  # to arg_stuff with the atom :false as an argument
  #ERROR: match
  accept_for :book, arg_stuff:false

  # weird that having an underscore in the keyword change the way it's parsed
  # in tree-sitter
  accept_for :book, arg:false

  #ERROR: match
  accept_for :book, arg_stuff: false, :name
end
