def foo():
  a = source1()
  if True:
     b = a
     b = sanitize1()
  else:
     b = a
  #ERROR:
  sink1(b)
