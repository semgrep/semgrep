def foo():
  a = source()
  if True:
     b = a
     b = sanitize()
  else:
     b = a
  sink(b)
