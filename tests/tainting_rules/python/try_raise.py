def foo():
  try:
    raise Foo()
    #OK:
    sink(source)
  except:
    #ruleid: test
    sink(source)
  #ruleid: test
  sink(source)
