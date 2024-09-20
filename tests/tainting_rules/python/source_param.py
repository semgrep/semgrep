def foo():
  source1(a, b, c)
  #ruleid: tainting
  sink1(a)
  #OK:
  sink1(b)
  #OK:
  sink1(c)
