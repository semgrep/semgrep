def foo():
  a = source()
  if cond():
     b = a
  #OK: tainting
  sink(b)
