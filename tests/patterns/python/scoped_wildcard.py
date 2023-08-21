
# match because wildcard imports do not respect scoping in matching
# ERROR:
foo(x)
# ERROR:
foo(A.x)

foo(y)

def foo():
  from A import *
  # ERROR:
  foo(x)
  # ERROR:
  foo(A.x)

  foo(y)