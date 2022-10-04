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
  # `a` doesn't really have labels P and Q at the same time,
  # this trigger because taint from both branches is unioned.
  #todook: tainting
  sink(a)
