def print_phrase(&block)
  #ERROR: match
  block.()
end

#ERROR: match
print_phrase { puts "Hello from block!" }
