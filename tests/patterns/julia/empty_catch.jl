# ERROR:
try
  x = 3
catch
  println("foo")
end

# don't match this, because it has an explicit catch.
# use an ellipsis if you want to match!
try
  x = 3
catch y
  println("foo")
end