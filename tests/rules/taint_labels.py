def foo():
  a = source()
  if cond():
     b = a
  #ruleid: tainting
  sink(b)
