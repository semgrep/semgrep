def foo():
  a = p()
  if cond():
     b = q(a)
  #ruleid: tainting
  sink(b)

def bar():
  a = p()
  #OK: tainting
  sink(a)

def baz():
  a = q()
  #OK: tainting
  sink(a)

def boo():
  if cond():
    a = p()
  else:
    a = q()
  #OK: tainting
  sink(a)
 