# ERROR:
try
  x = 3
catch
  println("foo")
end

# ERROR:
try
  x = 3
catch y
  println("foo")
end