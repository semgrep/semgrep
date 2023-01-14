# No security problem
def foo():
  a = source1()
  b = sanitize(a)
  #OK:
  sink1(b)
  #OK:
  sink(b)

# Should alarm- sanitize does not occur
def bar():
  a = source1()
  sanitize()
  #ERROR:
  eval(a)
  #ERROR:
  sink(a)

# No security problem
def baz():
  a = source1()
  b = sanitize()
  #OK:
  eval(b)
