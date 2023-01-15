def foo():
  a = source()
  b = a
  #OK: tainting
  sink(b)

def bar(x):
  a = source()
  b = a + x
  #OK: tainting
  sink(b)
