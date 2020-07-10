# No security problem
def foo():
  a = source1()
  b = sanitize(a)
  sink1(b)

# Should alarm- sanitize does not occur
def bar():
  a = source1()
  sanitize()
  eval(a)

# No security problem
def baz():
  a = source1()
  b = sanitize()
  eval(b)