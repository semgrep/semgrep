begin
  # do something
rescue RuntimeError, Timeout::Error => e
  # handling, possibly involving e
end
