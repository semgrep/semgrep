def foo(x):
  try:
    raise A from x
    #OK:
    sink(source)
  except:
    #ruleid: test
    sink(source)
  #ruleid: test
  sink(source)
