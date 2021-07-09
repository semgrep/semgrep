def foo():
  a = source1()
  b = "safe"
  #ERROR:
  sink1(a, b)
  #OK:
  sink1(b, a)
