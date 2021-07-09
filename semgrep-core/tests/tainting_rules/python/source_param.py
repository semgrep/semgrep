def foo():
  source1(a, b, c)
  #ERROR:
  sink1(a)
  #OK:
  sink1(b)
  #OK:
  sink1(c)
