def foo():
  a = source1()
  b = "safe"
  if c:
    #ERROR:
    return a
  else:
    #OK:
    return b
