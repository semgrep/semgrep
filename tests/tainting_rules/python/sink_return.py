def foo():
  a = source1()
  b = "safe"
  if c:
    #ruleid: tainting
    return a
  else:
    #OK:
    return b
