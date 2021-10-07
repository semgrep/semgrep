def foo():
  try:
    raise
    #OK:
    sink(source)
  except:
    #ERROR:
    sink(source)
  #ERROR:
  sink(source)
