def foo():
  a = source1()
  b = "safe"
  #ruleid: tainting
  sink1(a, b)
  #OK:
  sink1(b, a)
