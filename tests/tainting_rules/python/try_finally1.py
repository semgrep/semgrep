def foo():
  x = ok
  y = ok
  z = ok
  try:
    try:
      raise Foo()
      #OK:
      sink(source)
    finally:
      x = source
    y = x
  finally:
    #OK:
    sink(z)
    z = y
  #ruleid: test
  sink(z)
