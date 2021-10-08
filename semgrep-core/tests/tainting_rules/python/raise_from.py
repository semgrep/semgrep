def foo(x):
  try:
    raise A from x
    #OK:
    sink(source)
  except:
    #ERROR:
    sink(source)
  #ERROR:
  sink(source)
