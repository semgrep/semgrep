def foo():
  try:
    raise Foo()
    #OK:
    sink(source)
  except:
    #ERROR:
    sink(source)
  #ERROR:
  sink(source)
