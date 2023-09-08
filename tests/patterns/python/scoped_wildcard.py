
# no match because we only examine top-level wildcard imports
# TODO:
foo(x)
# ERROR:
foo(A.x)

foo(y)

def foo():
  from A import *
  # TODO:
  foo(x)
  # ERROR:
  foo(A.x)

  foo(y)