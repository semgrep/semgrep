def foo():
  a = source()
  if cond():
    b = a
    b = sanitize()
  else:
    b = a
  #todoruleid: tainting
  sink(b)

def bar():
  a = source()
  if cond():
    b = a
    b = sanitize()
  #OK: tainting
  sink(b)

def baz():
  a = source()
  if cond():
    b = a
  #ruleid: tainting
  sink(b)
