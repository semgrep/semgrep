def foo():
  try:
    raise
    #OK:
    sink(source)
  except:
    #ruleid: test
    sink(source)
  #ruleid: test
  sink(source)
